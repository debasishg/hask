{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified Data.Text as T
import qualified Money as Y

import           Control.Monad.Logger (runStdoutLoggingT)
import           Control.Monad.IO.Class
import           Control.Lens
import           Database.Persist.Sqlite (withSqlitePool)

import           Model.Account
import           Service.AccountService
import           Service.Banking

connectionString :: T.Text
connectionString = "/tmp/domain.db"

openConnections :: Int
openConnections = 3

main :: IO ()
main = runMigrateActions >> 
           openNewAccounts >>= 
               (\accs -> transferBehavior accs "0123456789" "1234567890" (400 :: Y.Dense "USD"))

-- main = runMigrateActions >> 
           -- openNewAccounts >>= 
               -- flip behavior "0123456789" 

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

                       maybeAcc       <- query pool ano
                       modified       <- maybe (fail $ "Invalid account " ++ show ano) 
                                               (runActionsForAccount [Credit (200 :: Y.Dense "USD"), Credit (400 :: Y.Dense "USD")]) 
                                               maybeAcc

                       insertOrUpdate pool modified 
                       query pool (modified ^. accountNo) >>= printResult

  where
    printResult (Just ac)  = print ac
    printResult Nothing = putStrLn "Not found"

execute :: T.Text -> IO ()
execute accountno = runStdoutLoggingT
  . withSqlitePool connectionString openConnections
    $ \pool -> liftIO $ netValueTransactionsForAccount pool accountno


-- | Sample use case
-- 1. add a bunch of accounts to the Database
-- 2. for the two account numbers specified, transfer specified amount from the first
--    account to the second
-- 3. store the updated accounts back to the database
-- 4. query that updated accounts and print the account details
transferBehavior :: [Account] -> T.Text -> T.Text -> Y.Dense "USD" -> IO ()
transferBehavior accounts fromAccount toAccount amount = runStdoutLoggingT
                     . withSqlitePool connectionString openConnections
                         $ \pool -> liftIO $ do
                               addAccounts pool accounts
                               transfer pool fromAccount toAccount amount 
                               query pool fromAccount >>= printResult
                               query pool toAccount >>= printResult
  where
    printResult (Just ac)  = print ac
    printResult Nothing = putStrLn "Not found"