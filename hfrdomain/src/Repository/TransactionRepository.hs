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

module Repository.TransactionRepository where 

import qualified Data.Text as T
import           Data.Int (Int64)
import           Data.Time
import           Data.Pool
import           Polysemy          
import           Polysemy.Input          
import           Database.Persist.Sqlite (SqlBackend)
import           Database.Persist.Sql (fromSqlKey)
import           Database.Persist (insert, insertMany_, selectList, (==.), (>=.), (<=.))

import           Model.Transaction
import           Model.Schema
import           Repository.SqliteUtils

-- | Repository abstraction that's independent of the underlying database 
-- representation
data TransactionRepository m a where
    QueryByAccount                        :: T.Text -> TransactionRepository m [Transaction]
    Store                                 :: Transaction -> TransactionRepository m Int64
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
  Store txn        -> runDB (fromSqlKey <$> insert txn)
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