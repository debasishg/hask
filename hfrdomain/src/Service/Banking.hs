{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Service.Banking where

import qualified Money as Y
import           Data.Text hiding (foldl')
import           Data.Foldable
import           Data.Pool
import           Data.Time
import           Data.Maybe (fromJust)
import           Validation (Validation (..) )
import           Polysemy
import           Polysemy.Input
import           Control.Applicative
import           Control.Monad.Trans.Maybe 
import           Control.Lens hiding (element)
import           Database.Persist.Sqlite (SqlBackend)
import           Database.Redis (Connection)

import           Model.Schema
import           Model.Account (isAccountClosed, balanceInCurrency)
import           Model.Transaction (makeTransaction)
import           Model.TransactionType
import qualified Repository.AccountRepository as AR
import qualified Repository.TransactionRepository as TR
import qualified Repository.AccountCache as AC

-- | a domain service for doing debit or credit transactions
-- | each transaction created will be added to transaction table and
-- | balance in account table will be updated depending on the type
-- | of the transaction
transact :: Pool SqlBackend 
    -> Connection 
    -> UTCTime 
    -> TransactionType 
    -> Text 
    -> UTCTime 
    -> Y.Dense "USD" 
    -> IO ()
transact conn rconn utcCurrent txnType ano txnDate amount = runAllEffects conn rconn doTransaction
  where 
    doTransaction = do 

      maybeAcc <- getAccount 

      maybe (error $ "Invalid account " ++ show ano) doDB maybeAcc
        where
          doDB acc = case makeTransaction utcCurrent Dr txnDate amount ano of
            Success txn -> TR.store txn >> AR.upsert (updatedBalance acc)
            Failure err -> error $ show err

          updatedBalance acc = case txnType of
             Dr -> acc & currentBalance %~ subtract amount
             Cr -> acc & currentBalance %~ (+ amount)

          getAccount = runMaybeT $
                MaybeT (AC.fetchCachedAccount ano) 
            <|> MaybeT (AR.queryAccount ano)

-- | Transfer amount from one account to another
-- | This is a cached service where we try to fetch accounts from the cache first and if that
-- | fails we fall back to the database. Similarly after update to database we update the cache
-- | with the updated accounts
-- |
-- | The function offers transfer in any currency - hence you need to pass the exchange rate with
-- | USD since account maintains balance in USD. If you want to pass amount in USD, pass 1 as the
-- | exchange rate
transfer :: Pool SqlBackend 
    -> Connection 
    -> Text 
    -> Text 
    -> Y.Dense target
    -> Y.ExchangeRate "USD" target
    -> IO ()
transfer conn rconn fromAccountNo toAccountNo amount exchangeRateWithUSD = 
  runAllEffects conn rconn doTransfer
    where 
      doTransfer = updateAccountBalances >>= \accs -> do
        AR.upsertMany accs
        AC.cacheAccounts accs 
          where
            updateAccountBalances = 
              updateBalances <$> getAccount fromAccountNo <*> getAccount toAccountNo
                where
                  getAccount = \ano -> runMaybeT $ 
                        MaybeT (AR.queryAccount ano) 
                    <|> MaybeT (AR.queryAccount ano)

                  updateBalances (Just fa) (Just ta)  = [fa & balanceInCurrency exchangeRateWithUSD %~ subtract amount, 
                                                         ta & balanceInCurrency exchangeRateWithUSD %~ (+ amount)]

                  updateBalances (Just _) Nothing     = error $ "To account [" ++ show toAccountNo ++ "] does not exist"
                  updateBalances Nothing (Just _)     = error $ "From account [" ++ show fromAccountNo ++ "] does not exist"
                  updateBalances Nothing Nothing      = error $ "From account [" ++ show fromAccountNo ++ "] and To account [" ++ show toAccountNo ++ "] does not exist"

-- | a domain service that accesses multiple repositories to fetch account and
-- | transactions, computes the net value of all transactions fetched and updates
-- | the balance in the account repository
netValueTransactionsForAccount :: Pool SqlBackend -> Connection -> Text -> IO ()
netValueTransactionsForAccount conn rconn ano = runAllEffects conn rconn doNetValueComputation
  where 
    zeroDollars = 0 :: Y.Dense "USD"
    doNetValueComputation = do

      -- try to get the account from cache, if not found, fetch from database
      maybeAcc  <- getAccount

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
          getAccount = runMaybeT $
                MaybeT (AC.fetchCachedAccount ano) 
            <|> MaybeT (AR.queryAccount ano)

          accountClosedErr closeDate = error ("Account number " ++ show ano ++ " is closed on " ++ show closeDate)
          computeNetValueAndUpdate acc = do
            txns <- TR.queryByAccount ano

            let amount = foldl' (+) zeroDollars (signValAmount <$> txns)
                updated = acc & currentBalance %~ (+ amount)

            AR.store updated
            AC.cacheAccount updated
              where 
                signValAmount txn = 
                  if txn ^. transactionType == Cr
                    then txn ^. transactionAmount
                    else (-1) * (txn ^. transactionAmount)

runAllEffects :: 
     Pool SqlBackend 
  -> Connection
  -> Sem '[AR.AccountRepository, TR.TransactionRepository, AC.AccountCache, Input (Pool SqlBackend), Input Connection, Embed IO] a 
  -> IO a

runAllEffects conn rconn program =
  program                    
    & AR.runAccountRepository
    & TR.runTransactionRepository
    & AC.runAccountCache
    & runInputConst conn  
    & runInputConst rconn  
    & runM
