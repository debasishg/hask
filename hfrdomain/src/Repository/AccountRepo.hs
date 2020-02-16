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

import qualified Money as Y
import qualified Data.Text as T
import           Data.Time
import           Data.Maybe (listToMaybe)
import           Polysemy          
import           Database.Persist (get, insert_, replace, selectList, (==.))
import           Database.Persist.Sqlite (SqlBackend, SqlPersistT)
import           Control.Monad.Logger (LoggingT)

import Model.Account
import Model.AccountType
import Model.Schema
import Repository.SqliteUtils

-- | Repository abstraction that's independent of the underlying database 
-- representation
data AccountRepo m a where
    QueryAccount         :: T.Text -> AccountRepo m (Maybe Account)
    -- Store                :: Account -> AccountRepository m ()
    -- QueryByOpenDate   :: UTCTime -> AccountRepository m [Account]
    -- AllAccounts       :: AccountRepository m [Account]
    -- Upsert            :: Account -> AccountRepository m Account

makeSem ''AccountRepo

-- runSqlPersist :: Member (Embed IO) r => Sem (SqlPersistT ': r) a -> Sem r a
runSqlPersist :: Member (Embed IO) r => T.Text -> Sem (SqlPersistT ': r) a -> Sem r a
runSqlPersist dbfile = interpretH $ (embed $ (runSqliteAction1 dbfile))

runAccountRepo :: (Member (Embed (SqlPersistT (LoggingT IO))) r) => Sem (AccountRepo ': r) a -> Sem r a
runAccountRepo = interpret $ \case
  QueryAccount ano -> runSqliteAction $ (get (AccountKey ano))