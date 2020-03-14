{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Banking where

import qualified Money as Y
import           Data.Text hiding (foldl')
import           Data.Foldable
import           Data.Pool
import           Data.Maybe (fromJust)
import           Polysemy
import           Polysemy.Input
import           Control.Lens hiding (element)
import           Database.Persist.Sqlite (SqlBackend)

import           Model.Schema
import           Model.Account (isAccountClosed)
import           Model.TransactionType
import qualified Repository.AccountRepository as AR
import qualified Repository.TransactionRepository as TR

-- | a domain service that accesses multiple repositories to fetch account and
-- | transactions, computes the net value of all transactions fetched and updates
-- | the balance in the account repository
netValueTransactionsForAccount :: Pool SqlBackend -> Text -> IO ()
netValueTransactionsForAccount conn ano = runAllEffects conn doNetValueComputation
  where 
    zeroDollars = 0 :: Y.Dense "USD"
    doNetValueComputation = do

      -- get the account from database
      maybeAcc  <- AR.queryAccount ano

      -- fail if the account does not exist
      -- if exists, check if the account is already closed
      let maybeCloseDate = maybe (fail $ "Invalid account " ++ show ano) 
                                 isAccountClosed 
                                 maybeAcc

      -- fail if the account is already closed
      -- otherwise compute net value of all transactions for the account and update
      maybe (computeNetValueAndUpdate $ fromJust maybeAcc) 
            accountClosedErr 
            maybeCloseDate

        where
          accountClosedErr closeDate = error ("Account number " ++ show ano ++ " is closed on " ++ show closeDate)
          computeNetValueAndUpdate acc = do
            txns <- TR.queryByAccount ano

            let amount = foldl' (+) zeroDollars (signValAmount <$> txns)
                updated = acc & currentBalance %~ (+ amount)

            AR.store updated
              where 
                signValAmount txn = 
                  if txn ^. transactionType == Cr
                    then txn ^. transactionAmount
                    else (-1) * (txn ^. transactionAmount)

runAllEffects :: 
     Pool SqlBackend 
  -> Sem '[AR.AccountRepository, TR.TransactionRepository, Input (Pool SqlBackend), Embed IO] a 
  -> IO a

runAllEffects conn program =
  program                    
    & AR.runAccountRepository
    & TR.runTransactionRepository
    & runInputConst conn  
    & runM
