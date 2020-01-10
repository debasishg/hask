{-# OPTIONS_GHC -foptimal-applicative-do #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module Account where

import Data.Maybe (fromJust, isNothing)
import Data.Time
import Data.Functor
import Data.Ratio
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Scientific (toBoundedInteger)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Validate
import Control.Monad.Reader
import Control.Lens hiding (element)
import Control.Lens.TH
import Data.Aeson (Object, Value(..), decode, FromJSON, ToJSON)
import Data.Aeson.QQ (aesonQQ)
import qualified Money as Y
import Money.Aeson
import GHC.Generics

data Account = Account {
    _accountNo          :: Text
  , _accountType        :: AccountType  
  , _accountHolderName  :: Text  
  , _accountOpenDate    :: UTCTime 
  , _accountCloseDate   :: Maybe UTCTime 
  , _currentBalance     :: Y.Dense "USD"
  , _rateOfInterest     :: Integer  -- 0 for checking account : validate
} deriving (Show)

data AccountType = Ch | Sv deriving (Show, Generic)
instance FromJSON AccountType
instance ToJSON AccountType

$(makeLenses ''Account)

newtype Env
  = Env {envPath :: [Text]}
  deriving (Show, Eq)

data Error = Error { errPath :: [Text], errInfo :: ErrorInfo }
  deriving (Show, Eq)
data ErrorInfo
  = JSONBadValue Text Value
  | JSONMissingKey Text
  | InvalidDateValueInJSON Text
  | InvalidMoneyValueInJSON Text
  | InvalidAccountNumber Text 
  | InvalidAccountName Text
  | InvalidAccountOpenDate Text
  | InvalidAccountOpenCloseDateCombination Text
  | AccountBalanceLessThanMinimumBalance Text
  | AccountAlreadyClosed Text UTCTime
  | InsufficientFundsInAccount Text
  | InvalidAccountType Text
  | RateNotApplicableForCheckingAccount
  deriving (Show, Eq)

-- | Smart constructor for making a Account from JSON data
makeAccount :: forall m. (MonadReader Env m, MonadValidate [Error] m) => Value -> IO (m Account)
makeAccount req = do 
  utcCurrent <- getCurrentTime  
  return $ withObject "request" req $ \o -> do

    accType         <- withKey o "account_type" parseAccountType
    accNo           <- withKey o "account_no" parseAccountNo
    accName         <- withKey o "account_name" parseAccountName
    accOpenDate     <- withKey o "account_open_date" (parseAccountOpenDate utcCurrent)
    accCloseDate    <- withKey o "account_close_date" (parseAccountCloseDate accOpenDate)
    currBalance     <- withKey o "account_current_balance" parseCurrentBalance
    rateOfInt       <- withKey o "rate_of_interest" (parseRateOfInterest accType)

    pure Account { _accountNo           = accNo, 
                   _accountType         = accType,
                   _accountHolderName   = accName, 
                   _accountOpenDate     = accOpenDate,
                   _accountCloseDate    = accCloseDate,
                   _currentBalance      = currBalance,
                   _rateOfInterest      = rateOfInt } where 

      parseAccountType :: Value -> m AccountType
      parseAccountType = asAccountType

      parseAccountNo v = do
        str <- asString v
        if T.length str /= 10
          then refuteErr $ InvalidAccountNumber str
          else pure str

      parseAccountName v = do
        str <- asString v
        if T.null str
          then refuteErr $ InvalidAccountName str
          else pure str

      parseAccountOpenDate curr v = do 
        dt       <- asDate v
        openDate <- tolerate $ validateAccountOpenDate curr dt
        pure $ fromJust openDate

      validateAccountOpenDate :: UTCTime -> UTCTime -> m UTCTime
      validateAccountOpenDate current d = 
        if current >= d 
          then pure d
          else refuteErr $ InvalidAccountOpenDate (T.pack $ "Account open date " ++ show d ++ " " ++ show current ++ " cannot be in future")

      parseAccountCloseDate :: UTCTime -> Value -> m (Maybe UTCTime)
      parseAccountCloseDate od v = do
        str <- asString v
        if T.null str
          then pure Nothing
          else Just <$> validateAccountCloseDate v od

      validateAccountCloseDate :: Value -> UTCTime -> m UTCTime
      validateAccountCloseDate v openDate = do
        dt <- asDate v 
        if dt < openDate
          then refuteErr $ InvalidAccountOpenCloseDateCombination (T.pack $ "Account close date " ++ show dt ++ " cannot precede open date " ++ show openDate)
          else pure dt

      parseCurrentBalance :: Value -> m (Y.Dense "USD")
      parseCurrentBalance v = do
        n <- asMoney v
        b <- tolerate $ validateCurrentBalance n
        pure $ fromJust b

      validateCurrentBalance :: Y.Dense "USD" -> m (Y.Dense "USD")
      validateCurrentBalance balance = 
        if balance >= (100 :: Y.Dense "USD")
          then pure balance
          else refuteErr $ AccountBalanceLessThanMinimumBalance (T.pack $ (show balance))

      parseRateOfInterest :: AccountType -> Value -> m Integer
      parseRateOfInterest Ch rate = asInteger rate >>= \i -> if i == 0 then pure i else refuteErr RateNotApplicableForCheckingAccount
      parseRateOfInterest Sv rate = asInteger rate

      pushPath :: Text -> m a -> m a
      pushPath path = local (\env -> env { envPath = path : envPath env })
      mkErr info = asks envPath <&> \path -> Error (Prelude.reverse path) info
      refuteErr = mkErr >=> \err -> refute [err]

      asString = \case { String s -> pure s;
                         v        -> refuteErr $ JSONBadValue "string" v }

      asDate = \case { String s -> (case (decode . L.fromStrict . encodeUtf8) s :: Maybe UTCTime of
                                      Nothing -> refuteErr $ InvalidDateValueInJSON s
                                      Just d -> pure d);
                       v        -> refuteErr $ JSONBadValue "date" v }

      asMoney = \case { String s -> (case (decode . L.fromStrict . encodeUtf8) s :: Maybe (Y.Dense "USD") of
                                       Nothing -> refuteErr $ InvalidMoneyValueInJSON s
                                       Just x  -> pure x);
                        v        -> refuteErr $ JSONBadValue "money" v }

      asAccountType = \case { String s -> (case (decode . L.fromStrict . encodeUtf8) s of 
                                             Nothing -> refuteErr $ InvalidAccountType s
                                             Just t  -> pure t);
                              v        -> refuteErr $ JSONBadValue "account-type" v }

      asNumber = \case { Number n -> pure n; v -> refuteErr $ JSONBadValue "number" v }
      asInteger v = asNumber v >>=
        maybe (refuteErr $ JSONBadValue "integer" v) (pure . toInteger) . toBoundedInteger @Int

      disputeErr :: ErrorInfo -> m ()
      disputeErr = mkErr >=> \err -> dispute [err]

      withObject :: Text -> Value -> (Object -> m a) -> m a
      withObject name v f = case v of { Object o -> f o; _ -> refuteErr $ JSONBadValue name v }

      withKey :: Object -> Text -> (Value -> m a) -> m a
      withKey o k f = maybe (refuteErr $ JSONMissingKey k) (pushPath k . f) $ M.lookup k o

-- | Check if an account is not closed
isAccountActive :: Account -> Bool 
isAccountActive account = isNothing $ account ^. accountCloseDate

-- | Check if the account is closed. If closed, return Maybe closeDate
-- else return Nothing
isAccountClosed :: Account -> Maybe UTCTime 
isAccountClosed account = 
  if isNothing $ account ^. accountCloseDate 
    then Nothing 
    else account ^. accountCloseDate

-- | Returns the number of days the account is open since the date passed
openDaysSince :: Account -> UTCTime -> Maybe NominalDiffTime
openDaysSince account sinceUTC =
  if not (isAccountActive account)
    then Nothing 
    else Just $ diffUTCTime (account ^. accountOpenDate) sinceUTC

-- | Close the bank account
-- Two checks to do:
-- a. if already closed, return error
-- b. if the passed in close date is Nothing, default to current date
close :: forall m. (MonadReader Env m, MonadValidate [ErrorInfo] m) => Account -> Maybe UTCTime -> IO (m Account)
close account maybeCloseDate = do
  utcCurrent <- getCurrentTime  
  pure (case isAccountClosed account of
    Just closedOn -> refute [AccountAlreadyClosed (account ^. accountNo) closedOn]
    Nothing       -> case maybeCloseDate of 
                        Just cd -> pure $ account & accountCloseDate .~ maybeCloseDate
                        Nothing -> pure $ account & accountCloseDate ?~ utcCurrent)

-- | Update the balance of an account after doing the following checks:
-- a. the account is active
-- b. if the amount passed is < 0 then ensure the debit does not violate min balance check
updateBalance :: forall m. (MonadReader Env m, MonadValidate [ErrorInfo] m) => Account -> Y.Dense "USD" -> m Account
updateBalance account amount = 
  checkBalance >>= \acc -> pure $ acc & currentBalance %~ (+ amount)
  where
    checkBalance =  
      case isAccountClosed account of
        Just closedOn -> refute [AccountAlreadyClosed (account ^. accountNo) closedOn]
        Nothing       -> if amount < 0 && (account ^. currentBalance + amount < 100)
                           then refute [InsufficientFundsInAccount (T.pack (show(account ^. currentBalance)))]
                           else pure account