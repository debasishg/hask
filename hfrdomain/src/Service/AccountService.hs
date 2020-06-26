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
    , transferInUSD
    ) where

import qualified Data.Text as T
import qualified Money as Y

import           Data.Pool
import           Data.Text hiding (map, foldr)
import           Data.Aeson.QQ (aesonQQ)
import           Data.Time
import           Control.Arrow
import           Control.Lens
import           Polysemy          
import           Polysemy.Input          
import           Database.Persist.Sqlite (SqlBackend, runMigration)
import           Data.List.NonEmpty hiding (map)
import           Validation (Validation (..), validationToEither)

import           Model.Account
import           Model.Schema
import           Errors
import           Combinators
import           Repository.SqliteUtils (runSqliteAction)
import           Repository.AccountRepository

-- | Some of the actions that can be done on an account, e.g. debit, credit,
-- | close, open, reopen etc.
data AccountAction = Debit !(Y.Dense "USD") 
                   | Credit !(Y.Dense "USD") 
                   | Close !UTCTime
                   deriving (Show, Read)

-- | a combinator for composing various actions on an Account
actionCombinator :: Account -> [AccountAction] -> Either (NonEmpty ErrorInfo) Account
actionCombinator account txns =
  let fns = map toFunction txns
  in runKleisli (flattenAndCompose $ map Kleisli fns) account 
  where 
    toFunction (Debit amt) = validationToEither . debit amt 
    toFunction (Credit amt) = validationToEither . credit amt 
    toFunction (Close closeDate) = validationToEither  . close closeDate

-- | a combinator that runs actions for a specific account number
runActionsForAccountNo :: Pool SqlBackend -> [AccountAction] -> Text -> IO (Either (NonEmpty ErrorInfo) Account)
runActionsForAccountNo conn actions accNo = do
  maybeAccount <- query conn accNo 
  return $ maybe (error "invalid account") (runActionsForAccount actions) maybeAccount

-- | a combinator that runs actions for a specific account 
runActionsForAccount :: [AccountAction] -> Account -> Either (NonEmpty ErrorInfo) Account
runActionsForAccount = flip actionCombinator 

-- | a combinator that runs actions for multiple accounts
runActionsForAccounts :: [AccountAction] -> [Account] -> Either (NonEmpty ErrorInfo) [Account]
runActionsForAccounts actions = mapM (runActionsForAccount actions) 

makeAccountAggregateFromContext :: Text -> Text -> Text -> IO (Validation (NonEmpty ErrorInfo) Account)
makeAccountAggregateFromContext accNo accType accountName = 
  accountFromJson [aesonQQ| { "account_no": #{accNo}, "account_type": #{accType}, "account_name": #{accountName}, "account_open_date": "\"1984-10-15T00:00:00Z\"", "account_close_date": "", "account_current_balance": "[\"USD\",3200,1]", "rate_of_interest": 2.5 } |]
    where
      accountFromJson inputValue = do
        utcCurrent <- getCurrentTime
        return $ makeAccount utcCurrent inputValue

openNewAccounts :: IO (Validation (NonEmpty ErrorInfo) [Account])
openNewAccounts = do 
  aggr1 <- makeAccountAggregateFromContext "0123456789" "\"Sv\"" "debasish"
  aggr2 <- makeAccountAggregateFromContext "1234567890" "\"Sv\"" "paramita"
  aggr3 <- makeAccountAggregateFromContext "2345678901" "\"Sv\"" "aarush"
  aggr4 <- makeAccountAggregateFromContext "3456789010" "\"Ch\"" "bhaja"
  return $ sequenceA [aggr1, aggr2, aggr3, aggr4]

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
transferInUSD :: Pool SqlBackend -> T.Text -> T.Text -> Y.Dense "USD" -> IO ()
transferInUSD conn fromAccountNo toAccountNo amount = 
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

-- | Transfer amount from one account to another using (maybe) a different currency
-- | You need to specify an exchange rate with USD for that
transfer :: Pool SqlBackend -> T.Text -> T.Text -> Y.Dense targetCCY -> Y.ExchangeRate "USD" targetCCY -> IO ()
transfer conn fromAccountNo toAccountNo amount exchangeRateWithUSD = 
  runAllEffects conn doTransfer
    where 
      doTransfer = 
        (updateBalances <$> 
              queryAccount fromAccountNo 
          <*> queryAccount toAccountNo) >>= upsertMany
          where
            updateBalances (Just fa) (Just ta)  = [fa & balanceInCurrency exchangeRateWithUSD %~ subtract amount, ta & balanceInCurrency exchangeRateWithUSD %~ (+ amount)]
            updateBalances (Just _) Nothing     = error $ "To account [" ++ show toAccountNo ++ "] does not exist"
            updateBalances Nothing (Just _)     = error $ "From account [" ++ show fromAccountNo ++ "] does not exist"
            updateBalances Nothing Nothing      = error $ "From account [" ++ show fromAccountNo ++ "] and To account [" ++ show toAccountNo ++ "] does not exist"
