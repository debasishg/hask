{-# OPTIONS_GHC -foptimal-applicative-do #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Application where

import qualified Money as Y
import           Data.Text
import           Data.Aeson.QQ (aesonQQ)
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Control.Monad.Validate
import           Database.Persist.Sqlite (runMigration)

import           Account
import           ValidateAeson
import           Schema
import           Repository.SqliteUtils
import           Repository.AccountRepository -- (AccountRepository)
import           Repository.SqliteAccountRepository ()

main :: IO [Account]
main = runMigrateActions 
  >>  makeNewAccounts 
  >>= makeTransactions 
  >>= doPersist

makeNewAccounts :: IO [Account]
makeNewAccounts = do 
  aggr1 <- runEitherT (makeAggregateFromContext "0123456789" "\"Sv\"" "debasish")
  aggr2 <- runEitherT (makeAggregateFromContext "1234567890" "\"Sv\"" "paramita")
  aggr3 <- runEitherT (makeAggregateFromContext "2345678901" "\"Sv\"" "aarush")
  either (fail . show) return $ sequence [aggr1, aggr2, aggr3]

makeTransactions :: [Account] -> IO [Account]
makeTransactions accounts = do
  as <- mapM (runEitherT . runDomainBehavior) accounts 
  either (fail . show) return $ sequence as

moretxns :: Text -> IO Account
moretxns accno = do
  maybeAccount <- doQuery accno 
  maybe (fail "Invalid account") runDomain maybeAccount
  where
    runDomain account = do
      result <- runEitherT $ runDomainBehavior account
      either (fail . show) return result 

makeAggregateFromContext :: Text -> Text -> Text -> EitherT [Error] IO Account
makeAggregateFromContext accNo accType accountName = 
  let env = Env []
      testcase input = do
        accRdr <- runValidateT <$> makeAccount input
        return $ runReader accRdr env

  in EitherT $ testcase [aesonQQ| { "account_no": #{accNo}, "account_type": #{accType}, "account_name": #{accountName}, "account_open_date": "\"1984-10-15T00:00:00Z\"", "account_close_date": "", "account_current_balance": "[\"USD\",3200,1]", "rate_of_interest": 2.5 } |]

runDomainBehavior :: Account -> EitherT [Error] IO Account
runDomainBehavior acc = 
  let env = Env []
      aggr = do 
        rdr <- runValidateT <$> composeBehaviors acc 
        return $ runReader rdr env

  in EitherT aggr

composeBehaviors :: forall m. (MonadReader Env m, MonadValidate [Error] m) => Account -> IO (m Account)
composeBehaviors acc = 
  return   $ credit (800 :: Y.Dense "USD") acc
         >>= debit (200 :: Y.Dense "USD") 
         >>= debit (100 :: Y.Dense "USD") 

doPersist :: [Account] -> IO [Account]
doPersist accounts =
  runSqliteAction $ mapM upsert accounts

runMigrateActions :: IO ()
runMigrateActions =
  runSqliteAction $ runMigration migrateAll

doQuery :: Text -> IO (Maybe Account)
doQuery account =
  runSqliteAction $ query account