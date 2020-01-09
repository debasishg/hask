{-# OPTIONS_GHC -foptimal-applicative-do #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module BankAccount where

import Data.Maybe (fromJust, isNothing)
import Data.Time
import Data.Functor
import Data.Ratio
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Validate
import Control.Monad.Reader
import Data.Aeson (Object, Value(..), decode)
import Data.Aeson.QQ (aesonQQ)
import qualified Money as Y
import Money.Aeson

data BankAccount = BankAccount {
    accountNo          :: Text
  , accountHolderName  :: Text  
  , accountOpenDate    :: UTCTime 
  , accountCloseDate   :: Maybe UTCTime 
  , currentBalance     :: Y.Dense "USD"
} deriving (Show)

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
  deriving (Show, Eq)

makeBankAccount :: forall m. (MonadReader Env m, MonadValidate [Error] m) => Value -> IO (m BankAccount)
makeBankAccount req = do 
  utcCurrent <- getCurrentTime  
  return $ withObject "request" req $ \o -> do

    accNo           <- withKey o "account_no" parseAccountNo
    accName         <- withKey o "account_name" parseAccountName
    accOpenDate     <- withKey o "account_open_date" (parseAccountOpenDate utcCurrent)
    accCloseDate    <- withKey o "account_close_date" (parseAccountCloseDate accOpenDate)
    currBalance     <- withKey o "account_current_balance" parseCurrentBalance

    pure BankAccount { accountNo           = accNo, 
                       accountHolderName   = accName, 
                       accountOpenDate     = accOpenDate,
                       accountCloseDate    = accCloseDate,
                       currentBalance      = currBalance } where

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

      disputeErr :: ErrorInfo -> m ()
      disputeErr = mkErr >=> \err -> dispute [err]

      withObject :: Text -> Value -> (Object -> m a) -> m a
      withObject name v f = case v of { Object o -> f o; _ -> refuteErr $ JSONBadValue name v }

      withKey :: Object -> Text -> (Value -> m a) -> m a
      withKey o k f = maybe (refuteErr $ JSONMissingKey k) (pushPath k . f) $ M.lookup k o

isAccountActive :: BankAccount -> Bool 
isAccountActive account = isNothing $ accountCloseDate account 

openDaysSince :: BankAccount -> UTCTime -> Maybe NominalDiffTime
openDaysSince account sinceUTC =
  if not (isAccountActive account)
    then Nothing 
    else Just $ diffUTCTime (accountOpenDate account) sinceUTC
