{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Repository.AccountRepo where 

import           Data.Function          ((&))
import qualified Data.Text as T
import           Data.Pool
import           Polysemy          
import           Polysemy.Input          
import           Database.Persist (get, insert_)
import           Database.Persist.Sqlite (SqlBackend, SqlPersistT, runSqlPool, withSqlitePool)
import           Control.Monad.Logger (runStdoutLoggingT)
import           Control.Monad.IO.Class
import           Control.Lens

import Model.Account
import Model.Schema

-- | Repository abstraction that's independent of the underlying database 
-- representation
data AccountRepo m a where
    QueryAccount         :: T.Text -> AccountRepo m (Maybe Account)
    Store                :: Account -> AccountRepo m ()
    -- QueryByOpenDate   :: UTCTime -> AccountRepository m [Account]
    -- AllAccounts       :: AccountRepository m [Account]
    -- Upsert            :: Account -> AccountRepository m Account

makeSem ''AccountRepo

runDB :: forall b r. Members [Embed IO, Input (Pool SqlBackend)] r => SqlPersistT IO b -> Sem r b
runDB action = embed . runSqlPool action =<< input

runAccountRepo :: forall r b. Members [Embed IO, Input (Pool SqlBackend)] r => Sem (AccountRepo ': r) b -> Sem r b
runAccountRepo = interpret $ \case
  QueryAccount ano -> runDB (get (AccountKey ano)) 
  Store acc -> runDB (insert_ acc)

runAllEffects :: Pool SqlBackend -> Sem '[AccountRepo, Input (Pool SqlBackend), Embed IO] a -> IO a
runAllEffects conn program =
  program                    
    & runAccountRepo
    & runInputConst conn  
    & runM

addAccount :: Pool SqlBackend -> Account -> IO ()
addAccount conn account = 
  runAllEffects conn (store account)

query :: Pool SqlBackend -> T.Text -> IO (Maybe Account)
query conn ano = 
  runAllEffects conn (queryAccount ano)

main :: Account -> IO ()
main a = runStdoutLoggingT 
             . withSqlitePool "/tmp/domain.db" 3 
                 $ \pool -> liftIO $ do
                     addAccount pool a 
                     query pool (a ^. accountNo) >>= printResult

  where
    printResult (Just ac)  = print ac
    printResult Nothing = putStrLn "Not found"

-- runMigrateActions >> openNewAccounts >>= \accs -> Repository.AccountRepo.main (Prelude.head accs)