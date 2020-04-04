{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.Text as T
import qualified Money as Y

import           Control.Monad.Logger (runStdoutLoggingT)
import           Control.Monad.IO.Class
import           Database.Persist.Sqlite (withSqlitePool)
import           Database.Redis (checkedConnect, defaultConnectInfo)
import           Validation (Validation (..))
import           Data.Time

import           Model.Account
import           Model.TransactionType
import           Service.AccountService
import           Service.Banking

connectionString :: T.Text
connectionString = "/tmp/domain.db"

openConnections :: Int
openConnections = 3

main :: IO ()
-- main = runMigrateActions >> 
--            openNewAccounts >>= \case 
--              Success accounts -> transferBehavior "0123456789" "1234567890" (400 :: Y.Dense "USD") accounts
--              Failure e        -> (error . show) e
--                -- transferBehavior accs "01238789" "1234890" (400 :: Y.Dense "USD")

main = runMigrateActions >> 
           openNewAccounts >>= \case
             Success accounts -> compositeBehavior accounts "0123456789" 
             Failure e        -> (error . show) e

-- | Sample use case
-- 1. add a bunch of accounts to the Database
-- 2. for the account number specified, run a series of actions (debit and credit)
-- 3. store the updated account back to the database
-- 4. query that updated account and print the account details
behavior :: [Account] -> T.Text -> IO ()
behavior accounts ano = runStdoutLoggingT 
             . withSqlitePool connectionString openConnections 
                 $ \pool -> liftIO $ do
                       addAccounts pool accounts 
                       modified       <- runActionsForAccountNo pool [Credit (200 :: Y.Dense "USD"), Credit (400 :: Y.Dense "USD")] ano
                       _              <- either (fail . show) (insertOrUpdate pool) modified 
                       query pool ano >>= printResult
  where
    printResult (Just ac)  = print ac
    printResult Nothing = putStrLn "Not found"
 
 -- | Sample use case
 -- 1. for the account numbr specified fetch all transactions and compute the net value
 -- 2. use the cache aware service netValueTransactionsForAccount
execute :: T.Text -> IO ()
execute accountno = runStdoutLoggingT
  . withSqlitePool connectionString openConnections
    $ \pool -> liftIO $ checkedConnect defaultConnectInfo 
        >>= \rconn -> liftIO $ netValueTransactionsForAccount pool rconn accountno


-- | Sample use case
-- 1. add a bunch of accounts to the Database
-- 2. for the two account numbers specified, transfer specified amount from the first
--    account to the second
-- 3. store the updated accounts back to the database
-- 4. query that updated accounts and print the account details
transferBehavior :: T.Text -> T.Text -> Y.Dense "USD" -> [Account] -> IO ()
transferBehavior fromAccount toAccount amount accounts= runStdoutLoggingT
                     . withSqlitePool connectionString openConnections
                         $ \pool -> liftIO $ do
                               addAccounts pool accounts
                               transferInUSD pool fromAccount toAccount amount 
                               query pool fromAccount >>= printResult
                               query pool toAccount >>= printResult
  where
    printResult (Just ac)  = print ac
    printResult Nothing = putStrLn "Not found"

-- | Sample use case
-- 1. add a bunch of accounts to the Database
-- 2. for the account number specified, run a series of transactions (debit and credit)
--    each of them will write transactions to db and update balance in account
-- 3. query that updated account and print the account details
compositeBehavior :: [Account] -> T.Text -> IO ()
compositeBehavior accounts ano = runStdoutLoggingT 
                      . withSqlitePool connectionString openConnections 
                          $ \pool -> liftIO $ do
                                rconn <- checkedConnect defaultConnectInfo 
                                utcCurrent <- getCurrentTime
                                addAccounts pool accounts 
                                _ <- transact pool rconn utcCurrent Cr ano utcCurrent (200 :: Y.Dense "USD")
                                _ <- transact pool rconn utcCurrent Cr ano utcCurrent (400 :: Y.Dense "USD")
                                _ <- transact pool rconn utcCurrent Dr ano utcCurrent (100 :: Y.Dense "USD")
                                query pool ano >>= printResult
  where
    printResult (Just ac)  = print ac
    printResult Nothing = putStrLn "Not found"
 