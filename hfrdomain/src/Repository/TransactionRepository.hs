{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Repository.TransactionRepository where

import qualified Data.Text as T
import qualified Data.Map  as M
import qualified Polysemy.State as S

import Data.Int (Int64)
import Data.Time ( UTCTime )
import Data.Pool ( Pool )
import Polysemy ( Member, Sem, Embed, Members, interpret, makeSem )
import Polysemy.Input ( Input )
import Database.Persist.Sqlite (SqlBackend)
import Database.Persist (insert_, insertMany_, selectList, (==.), (>=.), (<=.))
import Control.Lens ( (^.) )

import Model.Schema
    ( Transaction,
      EntityField(TransactionAccountNo, TransactionDate),
      transactionAccountNo,
      transactionDate,
      unEntity )
import Repository.SqliteUtils ( runDB )

-- | Repository abstraction that's independent of the underlying database 
-- representation
data TransactionRepository m a where
    QueryByAccount                        :: T.Text -> TransactionRepository m [Transaction]
    Store                                 :: Transaction -> TransactionRepository m ()
    StoreMany                             :: [Transaction] -> TransactionRepository m ()
    QueryByTransactionDate                :: UTCTime -> TransactionRepository m [Transaction]
    QueryByAccountNTransactionDateRange   :: T.Text -> UTCTime -> UTCTime -> TransactionRepository m [Transaction]

makeSem ''TransactionRepository

runTransactionRepository :: forall r b. Members [Embed IO, Input (Pool SqlBackend)] r => Sem (TransactionRepository ': r) b -> Sem r b
runTransactionRepository = interpret $ \case
  QueryByAccount ano -> runDB (doQueryByAccount ano)
    where
        doQueryByAccount no = do
            es <- selectList [TransactionAccountNo ==. no] []
            return $ unEntity <$> es
  Store txn        -> runDB (insert_ txn)
  StoreMany txns   -> runDB (insertMany_ txns)
  QueryByTransactionDate date -> runDB doQueryByTransactionDate
    where
      doQueryByTransactionDate = do
        es <- selectList [TransactionDate ==. date] []
        return $ unEntity <$> es
  QueryByAccountNTransactionDateRange ano start end -> runDB doQueryByAccountNTransactionDateRange
    where
      doQueryByAccountNTransactionDateRange = do
        es <- selectList [TransactionDate >=. start, TransactionDate <=. end, TransactionAccountNo ==. ano] []
        return $ unEntity <$> es

-- | Instance of the interpreter that can be used for testing
type TransactionMap = M.Map Int64 Transaction

runTransactionRepositoryInMemory :: forall r a. Member (S.State TransactionMap) r => Sem (TransactionRepository ': r) a -> Sem r a
runTransactionRepositoryInMemory = interpret $ \case
    QueryByAccount accountID                          -> S.gets (M.elems . M.filter (\t -> t ^. transactionAccountNo == accountID))
    Store txn                                         -> S.modify (\s -> M.insert (findMaxOr0 s + 1) txn s)
    StoreMany txns                                    -> S.modify (\s -> M.union (M.fromList ( (findMaxOr0 s + 1,) <$> txns) ) s)
    QueryByTransactionDate d                          -> S.gets (M.elems . M.filter (\t -> t ^. transactionDate == d))
    QueryByAccountNTransactionDateRange ano start end -> S.gets (
      M.elems . M.filter (\t ->
           t ^. transactionDate >= start
        && t ^. transactionDate <= end
        && t ^. transactionAccountNo == ano))
  where
    findMaxOr0 m =
      if M.null m
        then 0
        else fst (M.findMax m)