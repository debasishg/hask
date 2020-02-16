{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Database.SQLite.Simple as SQL
import           Data.Function          ((&))
import           Data.Text

import           Polysemy
import           Polysemy.Input
import           Control.Lens

import Model.Account
import Repository.AccountRepository

runAllEffects :: SQL.Connection
              -> (forall r. Member AccountRepository r => Sem r a)
              -> IO a
runAllEffects conn program =
  program                    
    & runAccountRepository   
    & runInputConst conn  
    & runM

dbFile :: FilePath
dbFile = "/tmp/account.db"

withPasswordDBConnection :: (SQL.Connection -> IO a) -> IO a
withPasswordDBConnection f = SQL.withConnection dbFile $ \conn -> do
  SQL.execute_ conn "CREATE TABLE IF NOT EXISTS account (account_no VARCHAR NOT NULL, \
                    \ account_type varchar NOT NULL, \
                    \ account_holder_name VARCHAR NOT NULL, \
                    \ account_open_date TIMESTAMP NOT NULL DEFAULT CURRENT_TIME, \
                    \ account_close_date TIMESTAMP NULL DEFAULT NULL, \
                    \ current_balance varchar NOT NULL, \
                    \ rate_of_interest REAL NOT NULL, PRIMARY KEY (account_no))"
  f conn

runAddAccount :: SQL.Connection -> Account -> IO ()
runAddAccount conn account = 
  runAllEffects conn (store account)

runQueryAccount :: SQL.Connection -> Text -> IO (Maybe Account)
runQueryAccount conn ano = 
  runAllEffects conn (queryAccount ano)

main :: Account -> IO ()
main a =
  withPasswordDBConnection $ \conn -> do
    putStrLn "Adding an account"
    runAddAccount conn a

    putStr "Querying an account"
    runQueryAccount conn (a ^. accountNo) >>= printResult

  where
    printResult (Just ac)  = print ac
    printResult Nothing = putStrLn "Not found"
