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

import           Data.Maybe (fromJust)
import           Data.Time
import           Data.Text.Encoding (encodeUtf8)
import           Data.Aeson (Value(..), decode)
import           Control.Monad.Validate
import           Control.Monad.Reader

import           Model.ValidateAeson
import           Model.Schema
import           Model.TransactionType
import           Errors

-- | Smart constructor for making a Account from JSON data
makeTransaction :: forall m. (MonadReader Env m, MonadValidate [Error] m) => Value -> IO (m Transaction)
makeTransaction req = do 
    utcCurrent <- getCurrentTime  
    return $ withObject "request" req $ \o -> do

        txnType       <- withKey o "transaction_type" parseTransactionType  
        txnDate       <- withKey o "transaction_date" (parseTransactionDate utcCurrent)
        txnAmount     <- withKey o "transaction_amount" parseTransactionAmount
        txnAccountNo  <- withKey o "transaction_account_no" parseTransactionAccountNo

        pure $ makeFullTransaction txnType txnDate txnAmount txnAccountNo
          where 
            parseTransactionType :: Value -> m TransactionType
            parseTransactionType = asTransactionType
      
            asTransactionType = \case { String s -> (case (decode . L.fromStrict . encodeUtf8) s of 
                                                      Nothing -> refuteErr $ InvalidTransactionType s
                                                      Just t  -> pure t);
                                        v        -> refuteErr $ JSONBadValue "transaction-type" v }

            parseTransactionAccountNo v = do
              str <- asString v
              if T.length str /= 10
                  then refuteErr $ InvalidAccountNumber str
                  else pure str
      
            parseTransactionDate curr v = do 
              dt       <- asDate v
              txnDate  <- tolerate $ validateTransactionDate curr dt
              pure $ fromJust txnDate
      
            validateTransactionDate :: UTCTime -> UTCTime -> m UTCTime
            validateTransactionDate current d = 
              if current >= d 
              then pure d
              else refuteErr $ InvalidTransactionDate (T.pack $ "Transaction date " ++ show d ++ " " ++ show current ++ " cannot be in future")
      
            parseTransactionAmount :: Value -> m (Y.Dense "USD")
            parseTransactionAmount v = do
              n <- asMoney v
              b <- tolerate $ validateTransactionAmount n
              pure $ fromJust b
      
            validateTransactionAmount :: Y.Dense "USD" -> m (Y.Dense "USD")
            validateTransactionAmount amt = 
              if amt >= (0 :: Y.Dense "USD")
              then pure amt
              else refuteErr $ TransactionAmountNegative (T.pack (show amt))
      