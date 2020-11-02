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

module Model.Transaction 
  (
      makeTransactionFromContext
    , makeTransaction
    , Transaction
  ) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Money as Y

import Data.Time ( UTCTime )
import Data.Text.Encoding (encodeUtf8)
import Data.Aeson (Value(..), decode)
import Validation (Validation (..), failure, failureIf)
import Data.List.NonEmpty ( NonEmpty, head )

import Model.ValidateAeson
    ( asDate, asMoney, asString, withKey, withObject )
import Model.Schema ( makeFullTransaction, Transaction )
import Model.TransactionType ( TransactionType )
import Errors
    ( ErrorInfo(TransactionAmountNegative, InvalidTransactionType,
                JSONBadValue, InvalidAccountNumber, InvalidTransactionDate) )

-- | Smart constructor for making a Transaction from JSON data
makeTransactionFromContext :: UTCTime -> Value -> Validation (NonEmpty ErrorInfo) Transaction
makeTransactionFromContext utcCurrent req = 
    withObject "request" req $ \o -> 
          makeFullTransaction 
      <$> withKey o "transaction_type" parseTransactionType 
      <*> withKey o "transaction_date" (parseTransactionDate utcCurrent)
      <*> withKey o "transaction_amount" parseTransactionAmount
      <*> withKey o "transaction_account_no" parseTransactionAccountNo

parseTransactionType :: Value -> Validation (NonEmpty ErrorInfo) TransactionType
parseTransactionType = asTransactionType
      
asTransactionType :: Value -> Validation (NonEmpty ErrorInfo) TransactionType
asTransactionType = \case { String s -> (case (decode . L.fromStrict . encodeUtf8) s of 
                                          Nothing -> failure $ InvalidTransactionType s
                                          Just t  -> Success t);
                            v        -> failure $ JSONBadValue "transaction-type" v }

parseTransactionAccountNo :: Value -> Validation (NonEmpty ErrorInfo) T.Text
parseTransactionAccountNo v = case asString v of
  Success str -> str <$ failureIf (T.length str /= 10) (InvalidAccountNumber str)
  Failure e   -> failure $ Data.List.NonEmpty.head e

parseTransactionDate :: UTCTime -> Value -> Validation (NonEmpty ErrorInfo) UTCTime
parseTransactionDate curr v = case asDate v of
  Success dt -> validateTransactionDate curr dt
  Failure e  -> failure $ Data.List.NonEmpty.head e
      
validateTransactionDate :: UTCTime -> UTCTime -> Validation (NonEmpty ErrorInfo) UTCTime
validateTransactionDate current d = d <$ failureIf (current < d) (InvalidTransactionDate (T.pack $ "Transaction date " ++ show d ++ " " ++ show current ++ " cannot be in future"))
      
parseTransactionAmount :: Value -> Validation (NonEmpty ErrorInfo) (Y.Dense "USD")
parseTransactionAmount v = case asMoney v of
  Success amt -> validateTransactionAmount amt
  Failure e   -> failure $ Data.List.NonEmpty.head e
      
validateTransactionAmount :: Y.Dense "USD" -> Validation (NonEmpty ErrorInfo) (Y.Dense "USD")
validateTransactionAmount amt = amt <$ failureIf (amt < (0 :: Y.Dense "USD")) (TransactionAmountNegative (T.pack (show amt)))

-- | Smart constructor for making a Transaction from individual fields
makeTransaction :: UTCTime 
    -> TransactionType
    -> UTCTime 
    -> Y.Dense "USD"
    -> T.Text
    -> Validation (NonEmpty ErrorInfo) Transaction
makeTransaction utcCurrent txnType txnDate txnAmount txnAccountNo =
      makeFullTransaction 
  <$> Success txnType
  <*> validateTransactionDate utcCurrent txnDate
  <*> validateTransactionAmount txnAmount
  <*> Success txnAccountNo