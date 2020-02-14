{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Repository.SqliteTransactionRepository where

import           Control.Monad.IO.Class (MonadIO)
import           Database.Persist (insert, selectList, (==.), (>=.), (<=.))
import           Database.Persist.Sql (fromSqlKey)
import           Database.Persist.Sqlite (SqlPersistT)

import           Repository.TransactionRepository
import           Model.Schema

-- | Instance of TransactionRepository for `SqlPersistT` (which is `ReaderT SqlBackend`)
instance (MonadIO m) => TransactionRepository (SqlPersistT m) where
  query ano = do
      es <- selectList [TransactionAccountNo ==. ano] []
      return $ unEntity <$> es

  store txn = fromSqlKey <$> insert txn

  queryByTransactionDate dt = do 
      es <- selectList [TransactionDate ==. dt] []
      return $ unEntity <$> es

  queryByAccountNTransactionDateRange ano start end = do
      es <- selectList [TransactionDate >=. start, TransactionDate <=. end, TransactionAccountNo ==. ano] []
      return $ unEntity <$> es