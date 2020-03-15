{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

-- | Exposes coarser level domain services in terms of actions that can be done on accounts.
-- The service layer publishes an ADT named `AccountAction` that models each action that can be
-- done on an account. The service layer also implements a combinator `actionCombinator` that
-- allows implementation of composition of multiple actions through a foldBind pattern.
module Service.AccountService 
    (
      AccountAction (Debit, Credit, Close)
    , actionCombinator
    , runActionsForAccount
    , runActionsForAccountNo
    , runActionsForAccounts
    , makeAccountAggregateFromContext
    , openNewAccounts
    , runMigrateActions
    , balanceInCurrency
    , addAccount
    , addAccounts
    , queryAllAccounts
    , query
    , queryAccountsByOpenDate
    , insertOrUpdate
    , transfer
    ) where

import qualified Data.Text as T
import qualified Money as Y

import           Data.Pool
import           Data.Text hiding (map, foldr)
import           Data.Aeson.QQ (aesonQQ)
import           Data.Time
import           Control.Arrow
import           Control.Lens
import           Control.Monad.Validate
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Polysemy          
import           Polysemy.Input          
import           Database.Persist.Sqlite (SqlBackend, runMigration)

import           Model.Account
import           Model.Schema
import           Errors
import           Combinators
import           Repository.SqliteUtils (runSqliteAction)
import           Repository.AccountRepository

-- | Some of the actions that can be done on an account, e.g. debit, credit,
-- close, open, reopen etc.
data AccountAction = Debit (Y.Dense "USD") 
                   | Credit (Y.Dense "USD") 
                   | Close UTCTime
                   deriving (Show, Read)

-- | a combinator for composing various actions on an Account
actionCombinator :: (MonadReader Env m, MonadValidate [Error] m) => Account -> [AccountAction] -> m Account
actionCombinator account txns =
  let fns = map toFunction txns
  -- in foldBinds (Prelude.head fns a) (Prelude.tail fns)
  in runKleisli (flattenAndCompose $ map Kleisli fns) account 
  where 
    toFunction (Debit amt) = debit amt 
    toFunction (Credit amt) = credit amt 
    toFunction (Close closeDate) = close closeDate 

-- | a combinator that runs actions for a specific account number
runActionsForAccountNo :: Pool SqlBackend -> [AccountAction] -> Text -> IO Account
runActionsForAccountNo conn actions accNo = do
  maybeAccount <- query conn accNo 
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

makeAccountAggregateFromContext :: Text -> Text -> Text -> EitherT [Error] IO Account
makeAccountAggregateFromContext accNo accType accountName = 
  let env = Env []
      testcase input = do
        accRdr <- runValidateT <$> makeAccount input
        return $ runReader accRdr env

  in EitherT $ testcase [aesonQQ| { "account_no": #{accNo}, "account_type": #{accType}, "account_name": #{accountName}, "account_open_date": "\"1984-10-15T00:00:00Z\"", "account_close_date": "", "account_current_balance": "[\"USD\",3200,1]", "rate_of_interest": 2.5 } |]

openNewAccounts :: IO [Account]
openNewAccounts = do 
  aggr1 <- runEitherT (makeAccountAggregateFromContext "0123456789" "\"Sv\"" "debasish")
  aggr2 <- runEitherT (makeAccountAggregateFromContext "1234567890" "\"Sv\"" "paramita")
  aggr3 <- runEitherT (makeAccountAggregateFromContext "2345678901" "\"Sv\"" "aarush")
  either (fail . show) return $ sequence [aggr1, aggr2, aggr3]

runMigrateActions :: IO ()
runMigrateActions =
  runSqliteAction $ runMigration migrateAll

runAllEffects :: Pool SqlBackend -> Sem '[AccountRepository, Input (Pool SqlBackend), Embed IO] a -> IO a
runAllEffects conn program =
  program                    
    & runAccountRepository
    & runInputConst conn  
    & runM

addAccount :: Pool SqlBackend -> Account -> IO ()
addAccount conn account = 
  runAllEffects conn (store account)

addAccounts :: Pool SqlBackend -> [Account] -> IO ()
addAccounts conn accounts = 
  runAllEffects conn (storeMany accounts)

query :: Pool SqlBackend -> T.Text -> IO (Maybe Account)
query conn ano = 
  runAllEffects conn (queryAccount ano)

queryAllAccounts :: Pool SqlBackend -> IO [Account]
queryAllAccounts conn =
  runAllEffects conn allAccounts

queryAccountsByOpenDate :: Pool SqlBackend -> UTCTime -> IO [Account]
queryAccountsByOpenDate conn dt =
  runAllEffects conn (queryByOpenDate dt)

insertOrUpdate :: Pool SqlBackend -> Account -> IO ()
insertOrUpdate conn acc = 
  runAllEffects conn (upsert acc)

-- | Transfer amount from one account to another
transfer :: Pool SqlBackend -> T.Text -> T.Text -> Y.Dense "USD" -> IO ()
transfer conn fromAccountNo toAccountNo amount = 
  runAllEffects conn doTransfer
    where 
      doTransfer = 
        (updateBalances <$> 
              queryAccount fromAccountNo 
          <*> queryAccount toAccountNo) >>= upsertMany
          where
            updateBalances (Just fa) (Just ta)  = [fa & currentBalance %~ subtract amount, ta & currentBalance %~ (+ amount)]
            updateBalances (Just _) Nothing     = error $ "To account [" ++ show toAccountNo ++ "] does not exist"
            updateBalances Nothing (Just _)     = error $ "From account [" ++ show fromAccountNo ++ "] does not exist"
            updateBalances Nothing Nothing      = error $ "From account [" ++ show fromAccountNo ++ "] and To account [" ++ show toAccountNo ++ "] does not exist"

-- | Gives a lens for getting / setting account balance in a specific currency
-- given the appropriate exchange rate
balanceInCurrency :: Y.ExchangeRate "USD" target -> Lens' Account (Y.Dense target)
balanceInCurrency usd2target = lens (getter usd2target) (setter $ Y.exchangeRateRecip usd2target)
  where
    getter :: Y.ExchangeRate "USD" target -> Account -> Y.Dense target
    getter exchangeRate account = 
      Y.exchange exchangeRate (account ^. currentBalance) 

    setter :: Y.ExchangeRate target "USD" -> Account -> Y.Dense target -> Account
    setter exchangeRate account amount = account & currentBalance %~ (+ Y.exchange exchangeRate amount)