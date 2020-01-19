{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Repository.SqliteAccountRepository where

import           Control.Monad.IO.Class (MonadIO)
import           Database.Persist (get, insert, selectList, (==.))
import           Database.Persist.Sqlite (SqlPersistT)

import           Repository.AccountRepository
import           Repository.Schema

-- | Instance of AccountRepository for `SqlPersistT` (which is `ReaderT SqlBackend`)
instance (MonadIO m) => AccountRepository (SqlPersistT m) where
  query ano = get (AccountKey ano)
  store acc = do
      _ <- insert acc
      return acc
  queryByOpenDate dt = do 
      es <- selectList [AccountOpenDate ==. dt] []
      return $ unEntity <$> es
  allAccounts = do
      es <- selectList [] []
      return $ unEntity <$> es