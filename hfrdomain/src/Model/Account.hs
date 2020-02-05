{-# OPTIONS_GHC -foptimal-applicative-do #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}

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

import           Data.Maybe (fromJust, isNothing)
import           Data.Time
import           Data.Text.Encoding (encodeUtf8)
import           Data.Aeson (Value(..), decode)
import           Control.Monad.Validate
import           Control.Monad.Reader
import           Control.Lens hiding (element)

import           Model.ValidateAeson
import           Model.Schema
import           Model.AccountType
import           Errors

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

        pure $ makeFullAccount accNo accType accName accOpenDate accCloseDate currBalance rateOfInt
          where 
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
              else refuteErr $ AccountBalanceLessThanMinimumBalance (T.pack (show balance))
      
            parseRateOfInterest :: AccountType -> Value -> m Double
            parseRateOfInterest Ch v = asDouble v >>= \i -> if i == 0.0 then pure i else refuteErr RateNotApplicableForCheckingAccount
            parseRateOfInterest Sv v = asDouble v
      
            asAccountType = \case { String s -> (case (decode . L.fromStrict . encodeUtf8) s of 
                                                   Nothing -> refuteErr $ InvalidAccountType s
                                                   Just t  -> pure t);
                                    v        -> refuteErr $ JSONBadValue "account-type" v }

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
close :: forall m. (MonadReader Env m, MonadValidate [Error] m) => UTCTime -> Account -> m Account
close closeDate account = maybe (pure canClose) alreadyClosed (isAccountClosed account)
    where
      alreadyClosed closedOn = refuteErr $ AccountAlreadyClosed (account ^. accountNo) closedOn
      canClose = account & accountCloseDate ?~ closeDate

-- | Update the balance of an account after doing the following checks:
-- a. the account is active
-- b. if the amount passed is < 0 then ensure the debit does not violate min balance check
updateBalance :: forall m. (MonadReader Env m, MonadValidate [Error] m) => Y.Dense "USD" -> Account -> m Account
updateBalance amount account = 
    checkBalance >>= \acc -> pure $ acc & currentBalance %~ (+ amount)
  where
    checkBalance = case isAccountClosed account of
        Just closedOn -> refuteErr $ AccountAlreadyClosed (account ^. accountNo) closedOn
        Nothing       -> if amount < 0 && (account ^. currentBalance + amount < 100)
                             then refuteErr $ InsufficientFundsInAccount (T.pack (show(account ^. currentBalance)))
                             else pure account

debit :: forall m. (MonadReader Env m, MonadValidate [Error] m) => Y.Dense "USD" -> Account -> m Account
debit amount = updateBalance ((-1) * amount) 

credit :: forall m. (MonadReader Env m, MonadValidate [Error] m) => Y.Dense "USD" -> Account -> m Account
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
