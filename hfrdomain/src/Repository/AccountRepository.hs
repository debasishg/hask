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
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Repository.AccountRepository where 

import qualified Data.Text as T

import           Data.Pool
import           Data.Time
import qualified Data.Map  as M

import           Control.Lens
import           Polysemy          
import           Polysemy.Input          
import qualified Polysemy.State as S
import           Database.Persist (get, insert_, insertMany_, repsert, selectList, (==.))
import           Database.Persist.Sqlite (SqlBackend)

import           Model.Account
import           Model.Schema
import           Repository.SqliteUtils

-- | Repository abstraction that's independent of the underlying database 
-- representation
data AccountRepository m a where
    QueryAccount         :: T.Text -> AccountRepository m (Maybe Account)
    Store                :: Account -> AccountRepository m ()
    StoreMany            :: [Account] -> AccountRepository m ()
    QueryByOpenDate      :: UTCTime -> AccountRepository m [Account]
    AllAccounts          :: AccountRepository m [Account]
    Upsert               :: Account -> AccountRepository m ()

makeSem ''AccountRepository

-- | Interpreter for AccountRepository
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
  QueryByOpenDate date -> runDB doQueryByOpenDate
    where
      doQueryByOpenDate = do 
        es <- selectList [AccountOpenDate ==. date] []
        return $ unEntity <$> es
  Upsert acc -> runDB doUpsert
    where
      doUpsert = repsert (AccountKey $ acc ^. accountNo) acc

-- | Instance of the interpreter that can be used for testing
type AccountMap = M.Map T.Text Account

runAccountRepositoryInMemory :: forall r a. Member (S.State AccountMap) r => Sem (AccountRepository ': r) a -> Sem r a
runAccountRepositoryInMemory = interpret $ \case
  QueryAccount accountID    -> S.gets (M.!? accountID)
  Store acc                 -> S.modify (M.insert (acc ^. accountNo) acc)
  StoreMany accs            -> S.modify (M.union (M.fromList ((\acc -> (acc ^. accountNo, acc)) <$> accs)))
  AllAccounts               -> S.gets M.elems
  QueryByOpenDate d         -> S.gets (M.elems . M.filter (\a -> a ^. accountOpenDate == d))
  Upsert acc                -> S.modify (M.insert (acc ^. accountNo) acc)