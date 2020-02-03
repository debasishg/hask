{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Exposes coarser level domain services in terms of actions that can be done on accounts.
-- The service layer publishes an ADT named `AccountAction` that models each action that can be
-- done on an account. The service layer also implements a combinator `actionCombinator` that
-- allows implementation of composition of multiple actions through a foldBind pattern.
module Service.AccountService 
    (
      persistAccounts
    , runQuery
    , AccountAction (Debit, Credit, Close)
    , actionCombinator
    , runActionsForAccountNo
    , runActionsForAccount
    , runActionsForAccounts
    , makeAggregateFromContext
    , openNewAccounts
    , runMigrateActions
    ) where

import qualified Money as Y
import           Data.Text hiding (map, foldr)
import           Data.Aeson.QQ (aesonQQ)
import           Data.Time
import           Control.Monad.Validate
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Database.Persist.Sqlite (runMigration)

import           Account
import           Schema
import           ValidateAeson
import           Repository.SqliteUtils (runSqliteAction)
import           Repository.AccountRepository
import           Repository.SqliteAccountRepository()

-- | Some of the actions that can be done on an account, e.g. debit, credit,
-- close, open, reopen etc.
data AccountAction = Debit (Y.Dense "USD") 
                   | Credit (Y.Dense "USD") 
                   | Close UTCTime
                   deriving (Show, Read)

-- | This is a generic pattern of processing a list of monadic functions through
-- `(>>=)`. `composeParts` just composes the Kleisli arrows through a `foldr`. 
-- And `foldBinds` binds them through. Adopted from the SoF thread https://stackoverflow.com/a/8717016
composeParts :: (Monad m) => [a -> m a] -> a -> m a
composeParts = foldr (>=>) return 

foldBinds :: (Monad m) => m a -> [a -> m a] -> m a
foldBinds m fs = m >>= composeParts fs

-- | a combinator for composing various actions on an Account
actionCombinator :: (MonadReader Env m, MonadValidate [Error] m) => Account -> [AccountAction] -> m Account
actionCombinator a txns =
  let fns = map toFunction txns
  in foldBinds (Prelude.head fns a) (Prelude.tail fns)
  where 
    toFunction (Debit amt) = debit amt 
    toFunction (Credit amt) = credit amt 
    toFunction (Close closeDate) = close closeDate 

-- | a combinator that runs actions for a specific account number
runActionsForAccountNo :: [AccountAction] -> Text -> IO Account
runActionsForAccountNo actions accNo = do
  maybeAccount <- runQuery accNo 
  maybe (fail "invalid account") (runActionsForAccount actions) maybeAccount

-- | a combinator that runs actions for a specific account 
runActionsForAccount :: [AccountAction] -> Account -> IO Account
runActionsForAccount actions account = do
  a <- (runEitherT . makeActions actions) account 
  either (fail . show) return a
  where
    makeActions :: [AccountAction] -> Account -> EitherT [Error] IO Account
    makeActions txns acc = 
      let env = Env []
          aggr = do 
            rdr <- runValidateT <$> composeActions acc txns
            return $ runReader rdr env

      in EitherT aggr
      where
        composeActions :: forall m. (MonadReader Env m, MonadValidate [Error] m) => Account -> [AccountAction] -> IO (m Account)
        composeActions a tns = 
          return $ actionCombinator a tns

-- | a combinator that runs actions for multiple accounts
runActionsForAccounts :: [AccountAction] -> [Account] -> IO [Account]
runActionsForAccounts actions = 
  mapM (runActionsForAccount actions) 

makeAggregateFromContext :: Text -> Text -> Text -> EitherT [Error] IO Account
makeAggregateFromContext accNo accType accountName = 
  let env = Env []
      testcase input = do
        accRdr <- runValidateT <$> makeAccount input
        return $ runReader accRdr env

  in EitherT $ testcase [aesonQQ| { "account_no": #{accNo}, "account_type": #{accType}, "account_name": #{accountName}, "account_open_date": "\"1984-10-15T00:00:00Z\"", "account_close_date": "", "account_current_balance": "[\"USD\",3200,1]", "rate_of_interest": 2.5 } |]

openNewAccounts :: IO [Account]
openNewAccounts = do 
  aggr1 <- runEitherT (makeAggregateFromContext "0123456789" "\"Sv\"" "debasish")
  aggr2 <- runEitherT (makeAggregateFromContext "1234567890" "\"Sv\"" "paramita")
  aggr3 <- runEitherT (makeAggregateFromContext "2345678901" "\"Sv\"" "aarush")
  either (fail . show) return $ sequence [aggr1, aggr2, aggr3]

runMigrateActions :: IO ()
runMigrateActions =
  runSqliteAction $ runMigration migrateAll

runQuery :: Text -> IO (Maybe Account)
runQuery account =
  runSqliteAction $ query account

persistAccounts :: [Account] -> IO [Account]
persistAccounts accounts =
  runSqliteAction $ mapM upsert accounts
