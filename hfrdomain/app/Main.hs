{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Main where

import qualified Data.Text as T
import qualified Money as Y

import           Data.Maybe (fromJust)
import           Control.Monad.Logger (runStdoutLoggingT)
import           Control.Monad.IO.Class
import           Control.Lens
import           Database.Persist.Sqlite (withSqlitePool)

import           Model.Account
import           Model.Schema
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
-- 2. for the account number specified, update the current balance by 100 USD
-- 3. store the updated account back to the database
-- 4. query that updated account and print the account details
behavior :: [Account] -> T.Text -> IO ()
behavior accounts ano = runStdoutLoggingT 
             . withSqlitePool connectionString openConnections 
                 $ \pool -> liftIO $ do
                       addAccounts pool accounts 
                       acc <- query pool ano
                       let modified = fromJust acc & currentBalance %~ (+ (100 :: Y.Dense "USD"))
                       updated <- insertOrUpdate pool modified 
                       query pool (updated ^. accountNo) >>= printResult

  where
    printResult (Just ac)  = print ac
    printResult Nothing = putStrLn "Not found"

execute :: T.Text -> IO ()
execute accountno = runStdoutLoggingT
  . withSqlitePool connectionString openConnections
    $ \pool -> liftIO $ netValueTransactionsForAccount pool accountno