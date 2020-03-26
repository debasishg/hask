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
      makeTransaction
    , Transaction
  ) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import qualified Money as Y

import           Data.Time
import           Data.Text.Encoding (encodeUtf8)
import           Data.Aeson (Value(..), decode)
import           Validation (Validation (..), failure, failureIf, validationToEither, eitherToValidation)
import           Data.List.NonEmpty

import           Model.ValidateAeson
import           Model.Schema
import           Model.TransactionType
import           Errors

-- | Smart constructor for making a Transaction from JSON data
makeTransaction :: UTCTime -> Value -> Validation (NonEmpty ErrorInfo) Transaction
makeTransaction utcCurrent req = 
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
parseTransactionAccountNo v = case validationToEither $ asString v of
  Right str -> str <$ failureIf (T.length str /= 10) (InvalidAccountNumber str)
  Left e    -> eitherToValidation (Left e)

parseTransactionDate :: UTCTime -> Value -> Validation (NonEmpty ErrorInfo) UTCTime
parseTransactionDate curr v = case validationToEither $ asDate v of
  Right dt -> validateTransactionDate curr dt
  Left  e  -> eitherToValidation (Left e)
      
validateTransactionDate :: UTCTime -> UTCTime -> Validation (NonEmpty ErrorInfo) UTCTime
validateTransactionDate current d = d <$ failureIf (current < d) (InvalidTransactionDate (T.pack $ "Transaction date " ++ show d ++ " " ++ show current ++ " cannot be in future"))
      
parseTransactionAmount :: Value -> Validation (NonEmpty ErrorInfo) (Y.Dense "USD")
parseTransactionAmount v = case validationToEither $ asMoney v of
  Right amt -> validateTransactionAmount amt
  Left  e   -> eitherToValidation (Left e)
      
validateTransactionAmount :: Y.Dense "USD" -> Validation (NonEmpty ErrorInfo) (Y.Dense "USD")
validateTransactionAmount amt = amt <$ failureIf (amt < (0 :: Y.Dense "USD")) (TransactionAmountNegative (T.pack (show amt)))