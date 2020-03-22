{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Model.Account 
    (
      makeAccount
    , isAccountActive
    , isAccountClosed
    , openDaysSince
    , credit
    , debit
    , interestFor
    , taxOnAccruedInterest
    , surchargeOnTax
    , feeFor
    , close
    , Account
    , accountNo
    , accountOpenDate
    , Env(..)
    ) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Money as Y
import qualified Data.HashMap.Strict as M
import           Data.Aeson (Value(..), decode, Object)
import           Validation (Validation (..), failure, validationToEither, eitherToValidation)
import           Data.List.NonEmpty
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time
import           GHC.TypeLits (KnownSymbol)
import           Data.Scientific (Scientific, toBoundedRealFloat)
import           Data.Maybe (isNothing, fromJust)
import           Control.Lens hiding (element)

import           Model.Schema
import           Model.AccountType
import           Errors

makeAccount :: UTCTime -> Value -> Validation (NonEmpty ErrorInfo) Account
makeAccount currentTime req = case validationToEither $ _makeAccount currentTime req of
  Right a -> validate a 
  Left  e -> eitherToValidation (Left e) 

_makeAccount :: UTCTime -> Value -> Validation (NonEmpty ErrorInfo) Account
_makeAccount utcCurrent req = 
    withObject "request" req $ \o -> 
          makeFullAccount 
      <$> withKey o "account_no" parseAccountNo
      <*> withKey o "account_type" parseAccountType
      <*> withKey o "account_name" parseAccountName
      <*> withKey o "account_open_date" (parseAccountOpenDate utcCurrent)
      <*> withKey o "account_close_date" parseAccountCloseDate 
      <*> withKey o "account_current_balance" parseCurrentBalance
      <*> withKey o "rate_of_interest" asDouble

validate :: Account -> Validation (NonEmpty ErrorInfo) Account
validate account = validateOpenCloseDate account *> validateRateOfInterest account

validateOpenCloseDate :: Account -> Validation (NonEmpty ErrorInfo) Account 
validateOpenCloseDate acc = case acc ^. accountCloseDate of
  Nothing -> Success acc 
  Just dt -> if acc ^. accountOpenDate <= dt
               then Success acc
               else failure $ InvalidAccountOpenCloseDateCombination (T.pack $ "Account close date " ++ show dt ++ " cannot precede open date " ++ show (acc ^. accountOpenDate))

parseAccountNo :: Value -> Validation (NonEmpty ErrorInfo) T.Text
parseAccountNo v = case validationToEither $ asString v of
  Right str -> validateAccountNumber str
  Left  str -> eitherToValidation (Left str)
      
validateAccountNumber :: T.Text -> Validation (NonEmpty ErrorInfo) T.Text
validateAccountNumber ano = 
  if T.length ano /= 10
    then failure $ InvalidAccountNumber ano
    else Success ano

asString :: Value -> Validation (NonEmpty ErrorInfo) T.Text
asString = \case { String s -> Success s;
                   v        -> failure $ JSONBadValue "string" v }

parseAccountType :: Value -> Validation (NonEmpty ErrorInfo) AccountType
parseAccountType = asAccountType

asAccountType :: Value -> Validation (NonEmpty ErrorInfo) AccountType
asAccountType = \case { String s -> (case (decode . L.fromStrict . encodeUtf8) s of 
                                       Nothing -> failure $ InvalidAccountType s
                                       Just t  -> Success t);
                        v        -> failure $ JSONBadValue "account-type" v }

parseAccountName :: Value -> Validation (NonEmpty ErrorInfo) T.Text
parseAccountName v = case validationToEither $ asString v of
  Right str -> validateAccountName str
  Left  str -> eitherToValidation (Left str)

validateAccountName :: T.Text -> Validation (NonEmpty ErrorInfo) T.Text
validateAccountName nm = 
  if T.null nm
      then failure $ InvalidAccountName nm
      else Success nm

parseAccountOpenDate :: UTCTime -> Value -> Validation (NonEmpty ErrorInfo) UTCTime
parseAccountOpenDate curr v = case validationToEither $ asDate v of
  Right dt -> validateAccountOpenDate curr dt
  Left  e  -> eitherToValidation (Left e)
      
validateAccountOpenDate :: UTCTime -> UTCTime -> Validation (NonEmpty ErrorInfo) UTCTime
validateAccountOpenDate current d = 
  if current >= d 
  then Success d
  else failure $ InvalidAccountOpenDate (T.pack $ "Account open date " ++ show d ++ " " ++ show current ++ " cannot be in future")

parseAccountCloseDate :: Value -> Validation (NonEmpty ErrorInfo) (Maybe UTCTime)
parseAccountCloseDate v = case validationToEither $ asString v of
  Left e    -> eitherToValidation (Left e)
  Right str -> if T.null str 
                 then Success Nothing
                 else Just <$> validateAccountCloseDate v 

validateAccountCloseDate :: Value -> Validation (NonEmpty ErrorInfo) UTCTime
validateAccountCloseDate v = case validationToEither $ asDate v of
  Left  e  -> eitherToValidation (Left e)
  Right dt -> Success dt

asDate :: Value -> Validation (NonEmpty ErrorInfo) UTCTime
asDate = \case { 
    String s -> (case (decode . L.fromStrict . encodeUtf8) s :: Maybe UTCTime of
                    Nothing -> failure $ InvalidDateValueInJSON s
                    Just d -> Success d);
    v        -> failure $ JSONBadValue "date" v }
      
parseCurrentBalance :: Value -> Validation (NonEmpty ErrorInfo)  (Y.Dense "USD")
parseCurrentBalance v = case validationToEither $ asMoney v of
  Right m -> validateCurrentBalance m
  Left  e -> eitherToValidation (Left e)

validateCurrentBalance :: Y.Dense "USD" -> Validation (NonEmpty ErrorInfo) (Y.Dense "USD")
validateCurrentBalance balance = 
  if balance >= (100 :: Y.Dense "USD")
  then Success balance
  else failure $ AccountBalanceLessThanMinimumBalance (T.pack (show balance))
      
asMoney :: (KnownSymbol c) => Value -> Validation (NonEmpty ErrorInfo) (Y.Dense c)
asMoney = \case { 
    String s -> (case (decode . L.fromStrict . encodeUtf8) s :: forall c1. (KnownSymbol c1) => Maybe (Y.Dense c1) of
                    Nothing -> failure $ InvalidMoneyValueInJSON s
                    Just x  -> Success x);
    v        -> failure $ JSONBadValue "money" v }

validateRateOfInterest :: Account -> Validation (NonEmpty ErrorInfo) Account
validateRateOfInterest acc = case acc ^. accountType of 
  Ch -> if acc ^. rateOfInterest == 0.0 then Success acc else failure RateNotApplicableForCheckingAccount
  Sv -> Success acc

asDouble :: Value -> Validation (NonEmpty ErrorInfo) Double
asDouble v = case validationToEither $ asNumber v of
  Left  e -> eitherToValidation (Left e)
  Right s -> case toBoundedRealFloat s of 
               Left  _i -> failure $ JSONBadValue "double" v
               Right  d -> Success d

asNumber :: Value -> Validation (NonEmpty ErrorInfo) Scientific
asNumber = \case { Number n -> Success n; v -> failure $ JSONBadValue "number" v }

-- | Check if an account is not closed
isAccountActive :: Account -> Bool 
isAccountActive account = isNothing $ account ^. accountCloseDate

-- | Check if the account is closed. If closed, return Maybe closeDate
-- else return Nothing
isAccountClosed :: Account -> Maybe UTCTime 
isAccountClosed account = account ^. accountCloseDate 

-- | Returns the number of days the account is open since the date passed
openDaysSince :: Account -> UTCTime -> Maybe NominalDiffTime
openDaysSince account sinceUTC =
    if not (isAccountActive account)
    then Nothing 
    else Just $ diffUTCTime (account ^. accountOpenDate) sinceUTC


-- | Close the bank account with the closeDate passed in. Checks if the account
-- is already closed, in which case it errors out
close :: UTCTime -> Account -> Validation (NonEmpty ErrorInfo) Account
close closeDate account = maybe (pure canClose) alreadyClosed (isAccountClosed account)
    where
      alreadyClosed closedOn = failure $ AccountAlreadyClosed (account ^. accountNo) closedOn
      canClose = account & accountCloseDate ?~ closeDate

-- | Update the balance of an account after doing the following checks:
-- a. the account is active
-- b. if the amount passed is < 0 then ensure the debit does not violate min balance check
updateBalance :: Y.Dense "USD" -> Account -> Validation (NonEmpty ErrorInfo) Account
updateBalance amount account = case validationToEither $ checkBalance of
  Right acc -> Success $ acc & currentBalance %~ (+ amount)
  Left  e   -> eitherToValidation (Left e)
  where
    checkBalance = case isAccountClosed account of
        Just closedOn -> failure $ AccountAlreadyClosed (account ^. accountNo) closedOn
        Nothing       -> if amount < 0 && (account ^. currentBalance + amount < 100)
                             then failure $ InsufficientFundsInAccount (T.pack (show(account ^. currentBalance)))
                             else Success account

debit :: Y.Dense "USD" -> Account -> Validation (NonEmpty ErrorInfo) Account
debit amount = updateBalance ((-1) * amount) 

credit :: Y.Dense "USD" -> Account -> Validation (NonEmpty ErrorInfo) Account
credit = updateBalance 

interestFor :: Account -> Y.Dense "USD"
interestFor acc = case acc ^. accountType of
  Ch -> 0 :: Y.Dense "USD"
  Sv -> fromJust $ Y.dense $ toRational (acc ^. currentBalance) * toRational (acc ^. rateOfInterest)

taxOnAccruedInterest :: Y.Dense "USD" -> Y.Dense "USD"
taxOnAccruedInterest interest = 
  if interest <= (100 :: Y.Dense "USD")
    then (0 :: Y.Dense "USD")
    else fromJust $ Y.dense $ toRational interest * (-0.01)

surchargeOnTax :: Y.Dense "USD" -> Y.Dense "USD"
surchargeOnTax tax = 
  if tax <= (100 :: Y.Dense "USD")
    then (0 :: Y.Dense "USD")
    else fromJust $ Y.dense $ toRational tax * (-0.01)

feeFor :: Account -> Y.Dense "USD" -> Y.Dense "USD"
feeFor acc balance = case acc ^. accountType of
  Ch -> 0 :: Y.Dense "USD"
  Sv -> if balance <= 1000
          then 0 :: Y.Dense "USD"
          else fromJust $ Y.dense $ toRational balance * (-0.01)

withObject :: forall a. T.Text -> Value -> (Object -> Validation (NonEmpty ErrorInfo) a) -> Validation (NonEmpty ErrorInfo) a
withObject name v f = case v of { Object o -> f o; _ -> failure $ JSONBadValue name v }

withKey :: forall a. Object -> T.Text -> (Value -> Validation (NonEmpty ErrorInfo) a) -> Validation (NonEmpty ErrorInfo) a
withKey o k f = maybe (failure $ JSONMissingKey k) f $ M.lookup k o
