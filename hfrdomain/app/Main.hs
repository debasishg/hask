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
               flip behavior "0123456789" 

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