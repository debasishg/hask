{-# LANGUAGE DataKinds #-}

module Service.TransactionService where

import qualified Money as Y
import           Data.Text hiding (foldl')
import           Data.Time
import           Data.Maybe
import           Data.Foldable
import           Data.Aeson (Value(..))
import           Data.Pool
import           Data.List.NonEmpty
import           Control.Lens hiding (element)
import           Database.Persist.Sqlite (SqlBackend)
import           Polysemy          
import           Polysemy.Input          
import           Validation (Validation (..))

import           Model.Transaction
import           Model.TransactionType
import           Model.Schema
import           Repository.TransactionRepository
import           Errors

zeroDollars :: Y.Dense "USD"
zeroDollars = 0 :: Y.Dense "USD"

-- | Make a Transaction from a json value
makeTransactionAggregateFromContext :: Value -> IO (Validation.Validation (NonEmpty ErrorInfo) Transaction)
makeTransactionAggregateFromContext jsonValue = do
  utcCurrent <- getCurrentTime
  return $ makeTransactionFromContext utcCurrent jsonValue

runAllEffects :: Pool SqlBackend -> Sem '[TransactionRepository, Input (Pool SqlBackend), Embed IO] a -> IO a
runAllEffects conn program =
  program                    
    & runTransactionRepository
    & runInputConst conn  
    & runM

addTransaction :: Pool SqlBackend -> Transaction -> IO ()
addTransaction conn txn = 
  runAllEffects conn (store txn)

addTransactions :: Pool SqlBackend -> [Transaction] -> IO ()
addTransactions conn txns =
  runAllEffects conn (storeMany txns)

getTransactionsByAccount :: Pool SqlBackend -> Text -> IO [Transaction]
getTransactionsByAccount conn ano =
  runAllEffects conn (queryByAccount ano)

getTransactionsByTransactionDate :: Pool SqlBackend -> UTCTime -> IO [Transaction]
getTransactionsByTransactionDate conn date =
  runAllEffects conn (queryByTransactionDate date)

getTransactionsByAccountNTransactionDateRange :: Pool SqlBackend -> Text -> UTCTime -> UTCTime -> IO [Transaction]
getTransactionsByAccountNTransactionDateRange conn ano start end =
  runAllEffects conn (queryByAccountNTransactionDateRange ano start end)

-- | Gets the list of transactions starting from beginning to 
-- the `asOn` date
allTransactionsAsOn :: Pool SqlBackend -> Text -> UTCTime -> IO [Transaction]
allTransactionsAsOn conn aNo = 
  getTransactionsByAccountNTransactionDateRange 
      conn 
      aNo 
      (UTCTime (fromJust $ fromGregorianValid 2000 01 01) 0) 

-- | Gets the algebraic value of all transaction amounts summed over the
-- date range from beginning till `asOn`. Debits are considered negative
-- while credits positive. Can't use `foldMap` as `Money` doesn't have a 
-- `Monoid` instance
balanceAsOn :: Pool SqlBackend -> Text -> UTCTime -> IO (Y.Dense "USD")
balanceAsOn conn aNo asOn = do
  txns <- allTransactionsAsOn conn aNo asOn
  return $ foldl' (+) zeroDollars (signValAmount <$> txns)
    where 
      signValAmount txn = 
        if txn ^. transactionType == Cr
          then txn ^. transactionAmount
          else (-1) * (txn ^. transactionAmount)