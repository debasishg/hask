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
import qualified Data.Map  as M
import qualified Polysemy.State as S
import Data.Pool ( Pool )
import Data.Time ( UTCTime )
import Control.Lens ( (^.) )
import Polysemy ( Member, Sem, Embed, Members, interpret, makeSem )
import Polysemy.Input ( Input )
import Database.Persist (get, insert_, insertMany_, repsert, repsertMany, selectList, (==.))
import Database.Persist.Sqlite (SqlBackend)

import Model.Schema
    ( accountNo,
      accountOpenDate,
      Account,
      EntityField(AccountOpenDate),
      Key(AccountKey),
      unEntity,
      AccountKey )
import Repository.SqliteUtils ( runDB )

-- | Repository abstraction that's independent of the underlying database 
-- representation
data AccountRepository m a where
    QueryAccount         :: T.Text -> AccountRepository m (Maybe Account)
    Store                :: Account -> AccountRepository m ()
    StoreMany            :: [Account] -> AccountRepository m ()
    QueryByOpenDate      :: UTCTime -> AccountRepository m [Account]
    AllAccounts          :: AccountRepository m [Account]
    Upsert               :: Account -> AccountRepository m ()
    UpsertMany           :: [Account] -> AccountRepository m ()

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
  UpsertMany accs -> runDB doUpsert
    where
      doUpsert = repsertMany $ (\acc -> (AccountKey $ acc ^. accountNo, acc)) <$> accs


-- | Instance of the interpreter that can be used for testing
type AccountMap = M.Map AccountKey Account

runAccountRepositoryInMemory :: forall r a. Member (S.State AccountMap) r => Sem (AccountRepository ': r) a -> Sem r a
runAccountRepositoryInMemory = interpret $ \case
  QueryAccount accountID    -> S.gets (M.!? AccountKey accountID)
  Store acc                 -> S.modify (M.insert (AccountKey (acc ^. accountNo)) acc)
  StoreMany accs            -> S.modify (M.union (M.fromList ((\acc -> (AccountKey(acc ^. accountNo), acc)) <$> accs)))
  AllAccounts               -> S.gets M.elems
  QueryByOpenDate d         -> S.gets (M.elems . M.filter (\a -> a ^. accountOpenDate == d))
  Upsert acc                -> S.modify (M.insert (AccountKey (acc ^. accountNo)) acc)
  UpsertMany accs           -> S.modify (M.union (M.fromList ((\acc -> (AccountKey(acc ^. accountNo), acc)) <$> accs)))