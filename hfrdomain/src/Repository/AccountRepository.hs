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

module Repository.AccountRepository where 

import qualified Data.Text as T

import           Data.Pool
import           Data.Time
import           Control.Lens
import           Polysemy          
import           Polysemy.Input          
import           Database.Persist (get, insert_, insertMany_, replace, selectList, (==.))
import           Database.Persist.Sqlite (SqlBackend, SqlPersistT, runSqlPool)

import           Model.Account
import           Model.Schema

-- | Repository abstraction that's independent of the underlying database 
-- representation
data AccountRepository m a where
    QueryAccount         :: T.Text -> AccountRepository m (Maybe Account)
    Store                :: Account -> AccountRepository m ()
    StoreMany            :: [Account] -> AccountRepository m ()
    QueryByOpenDate      :: UTCTime -> AccountRepository m [Account]
    AllAccounts          :: AccountRepository m [Account]
    Upsert               :: Account -> AccountRepository m Account

makeSem ''AccountRepository

runDB :: forall b r. Members [Embed IO, Input (Pool SqlBackend)] r => SqlPersistT IO b -> Sem r b
runDB action = embed . runSqlPool action =<< input

runAccountRepository :: forall r b. Members [Embed IO, Input (Pool SqlBackend)] r => Sem (AccountRepository ': r) b -> Sem r b
runAccountRepository = interpret $ \case
  QueryAccount ano -> runDB (get (AccountKey ano)) 
  Store acc        -> runDB (insert_ acc)
  StoreMany accs   -> runDB (insertMany_ accs)
  AllAccounts      -> runDB doAllAccounts
    where 
      doAllAccounts = do
        es <- selectList [] []
        return $ unEntity <$> es
  QueryByOpenDate date -> runDB (doQueryByOpenDate date)
    where
      doQueryByOpenDate dt = do 
        es <- selectList [AccountOpenDate ==. dt] []
        return $ unEntity <$> es
  Upsert acc -> runDB (doUpsert acc)
    where
      doUpsert ac = do 
        a <- get (AccountKey $ ac ^. accountNo)
        _ <- maybe (insert_ ac) (replace (AccountKey $ ac ^. accountNo)) a
        return ac
