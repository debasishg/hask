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
    , balanceInCurrency
    , Env(..)
    ) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Money as Y
import           Data.Aeson (Value(..), decode)
import           Validation (Validation (..), failure, failureIf)
import           Data.List.NonEmpty
import           Data.Text.Encoding (encodeUtf8)
import           Data.Time
import           Data.Maybe (isNothing, fromJust)
import           Control.Lens hiding (element)

import           Model.Schema
import           Model.AccountType
import           Model.ValidateAeson
import           Errors

makeAccount :: UTCTime -> Value -> Validation (NonEmpty ErrorInfo) Account
makeAccount currentTime req = case _makeAccount currentTime req of
  Success a -> validate a 
  Failure e -> failure $ Data.List.NonEmpty.head e

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
  Just dt -> acc <$ failureIf (acc ^. accountOpenDate > dt) (InvalidAccountOpenCloseDateCombination (T.pack $ "Account close date " ++ show dt ++ " cannot precede open date " ++ show (acc ^. accountOpenDate)))

parseAccountNo :: Value -> Validation (NonEmpty ErrorInfo) T.Text
parseAccountNo v = case asString v of
  Success str -> validateAccountNumber str
  Failure str -> failure $ Data.List.NonEmpty.head str
      
validateAccountNumber :: T.Text -> Validation (NonEmpty ErrorInfo) T.Text
validateAccountNumber ano = ano <$ failureIf (T.length ano /= 10) (InvalidAccountNumber ano)

parseAccountType :: Value -> Validation (NonEmpty ErrorInfo) AccountType
parseAccountType = asAccountType

asAccountType :: Value -> Validation (NonEmpty ErrorInfo) AccountType
asAccountType = \case { String s -> (case (decode . L.fromStrict . encodeUtf8) s of 
                                       Nothing -> failure $ InvalidAccountType s
                                       Just t  -> Success t);
                        v        -> failure $ JSONBadValue "account-type" v }

parseAccountName :: Value -> Validation (NonEmpty ErrorInfo) T.Text
parseAccountName v = case asString v of
  Success str -> validateAccountName str
  Failure str -> failure $ Data.List.NonEmpty.head str

validateAccountName :: T.Text -> Validation (NonEmpty ErrorInfo) T.Text
validateAccountName nm = nm <$ failureIf (T.null nm) (InvalidAccountName nm)

parseAccountOpenDate :: UTCTime -> Value -> Validation (NonEmpty ErrorInfo) UTCTime
parseAccountOpenDate curr v = case asDate v of
  Success dt -> validateAccountOpenDate curr dt
  Failure e  -> failure $ Data.List.NonEmpty.head e
      
validateAccountOpenDate :: UTCTime -> UTCTime -> Validation (NonEmpty ErrorInfo) UTCTime
validateAccountOpenDate current d = d <$ failureIf (current < d) (InvalidAccountOpenDate (T.pack $ "Account open date " ++ show d ++ " " ++ show current ++ " cannot be in future"))

parseAccountCloseDate :: Value -> Validation (NonEmpty ErrorInfo) (Maybe UTCTime)
parseAccountCloseDate v = case asString v of
  Failure e   -> failure $ Data.List.NonEmpty.head e
  Success str -> if T.null str 
                 then Success Nothing
                 else Just <$> validateAccountCloseDate v 

validateAccountCloseDate :: Value -> Validation (NonEmpty ErrorInfo) UTCTime
validateAccountCloseDate v = case asDate v of
  Failure e -> failure $ Data.List.NonEmpty.head e
  Success dt -> Success dt

parseCurrentBalance :: Value -> Validation (NonEmpty ErrorInfo)  (Y.Dense "USD")
parseCurrentBalance v = case asMoney v of
  Success m -> validateCurrentBalance m
  Failure e -> failure $ Data.List.NonEmpty.head e

validateCurrentBalance :: Y.Dense "USD" -> Validation (NonEmpty ErrorInfo) (Y.Dense "USD")
validateCurrentBalance balance = 
  balance <$ failureIf (balance < (100 :: Y.Dense "USD")) (AccountBalanceLessThanMinimumBalance (T.pack (show balance)))
      
validateRateOfInterest :: Account -> Validation (NonEmpty ErrorInfo) Account
validateRateOfInterest acc = case acc ^. accountType of 
  Ch -> acc <$ failureIf (acc ^. rateOfInterest == 0.0) RateNotApplicableForCheckingAccount
  Sv -> Success acc

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
updateBalance amount account = case checkBalance of
  Success acc -> Success $ acc & currentBalance %~ (+ amount)
  Failure e   -> failure $ Data.List.NonEmpty.head e
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

-- | Gives a lens for getting / setting account balance in a specific currency
-- | given the appropriate exchange rate
balanceInCurrency :: Y.ExchangeRate "USD" target -> Lens' Account (Y.Dense target)
balanceInCurrency usd2target = lens (getter usd2target) (setter $ Y.exchangeRateRecip usd2target)
  where
    getter :: Y.ExchangeRate "USD" target -> Account -> Y.Dense target
    getter exchangeRate account = 
      Y.exchange exchangeRate (account ^. currentBalance) 

    setter :: Y.ExchangeRate target "USD" -> Account -> Y.Dense target -> Account
    setter exchangeRate account amount = account & currentBalance %~ (+ Y.exchange exchangeRate amount)
