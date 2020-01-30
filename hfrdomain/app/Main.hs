{-# OPTIONS_GHC -foptimal-applicative-do #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Money as Y
import           Data.Text
import           Data.Time
import           Data.Aeson.QQ (aesonQQ)
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Control.Monad.Validate
import           Control.Lens hiding (element)
import           Database.Persist.Sqlite (runMigration)

import           Account
import           ValidateAeson
import           Schema
import           Repository.SqliteUtils
import           Repository.AccountRepository -- (AccountRepository)
import           Repository.SqliteAccountRepository ()
import           Service.AccountService

main :: IO ()
main = do 
  aggregate <- runEitherT $ makeAggregateFromContext >>= runDomainBehavior
  case aggregate of
    Left err      -> print $ show err
    Right account -> runRepositoryActions account

step1 :: IO Account
step1 = do 
  aggregate <- runEitherT $ makeAggregateFromContext >>= runDomainBehavior
  either (fail . show) runStoreRepositoryActions aggregate

step2 :: Text -> IO Account
step2 accno = do
  maybeAccount <- runQueryRepositoryActions accno 
  maybe (fail "Invalid account") runDomain maybeAccount
  where
    runDomain account = do
      result <- runEitherT $ runDomainBehavior account
      either (fail . show) return result 

composite :: IO Account
composite = step1 >>= \a -> step2 (a ^. accountNo)

--          IO Account -> (Account -> IO Account) -> Maybe Account -> IO Account
-- maybe :: b          -> (a -> b)                -> Maybe a       -> b
-- either :: (a -> c) -> (b -> c) -> Either a b -> c


makeAggregateFromContext :: EitherT [Error] IO Account
makeAggregateFromContext = 
  let env = Env []
      testcase input = do
        accRdr <- runValidateT <$> makeAccount input
        return $ runReader accRdr env

  in EitherT $ testcase [aesonQQ| { "account_no": "1234567890", "account_type": "\"Sv\"", "account_name": "debasish", "account_open_date": "\"1984-10-15T00:00:00Z\"", "account_close_date": "", "account_current_balance": "[\"USD\",3200,1]", "rate_of_interest": 2.5 } |]

runDomainBehavior :: Account -> EitherT [Error] IO Account
runDomainBehavior acc = 
  let env = Env []
      aggr = do 
        rdr <- runValidateT <$> composeBehaviors acc 
        return $ runReader rdr env

  in EitherT aggr

composeBehaviors :: forall m. (MonadReader Env m, MonadValidate [Error] m) => Account -> IO (m Account)
composeBehaviors acc = do
  curr <- getCurrentTime

  return   $ credit (800 :: Y.Dense "USD") acc
         >>= debit (200 :: Y.Dense "USD") 
         >>= debit (100 :: Y.Dense "USD") 
         -- >>= close curr

runDomainServices :: Text -> EitherT [Error] IO Account
runDomainServices acc = 
  let env = Env []
      aggr = do 
        rdr <- runValidateT <$> composeServices acc 
        return $ runReader rdr env

  in EitherT aggr

composeServices :: forall m. (MonadReader Env m, MonadValidate [Error] m, AccountRepository m) => Text -> IO (m Account)
composeServices acc = do 
  curr <- getCurrentTime

  return   $ creditAccount (800 :: Y.Dense "USD") acc
         >>= \a -> debitAccount (200 :: Y.Dense "USD") (a ^. accountNo)
         >>= \b -> debitAccount (100 :: Y.Dense "USD") (b ^. accountNo)
         >>= close curr

runRepositoryActions :: Account -> IO ()
runRepositoryActions account =
  runSqliteAction $ do
    _ <- runMigration migrateAll
    b <- store account
    liftIO $ print b

runStoreRepositoryActions :: Account -> IO Account
runStoreRepositoryActions account =
  runSqliteAction $ do
    _ <- runMigration migrateAll
    store account

runQueryRepositoryActions :: Text -> IO (Maybe Account)
runQueryRepositoryActions account =
  runSqliteAction $ query account