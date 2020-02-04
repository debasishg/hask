{-# LANGUAGE FlexibleInstances    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Repository.SqliteAccountRepository where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Lens hiding (element)
import           Database.Persist (get, insert_, replace, selectList, (==.))
import           Database.Persist.Sqlite (SqlPersistT)

import           Repository.AccountRepository
import           Model.Schema

-- | Instance of AccountRepository for `SqlPersistT` (which is `ReaderT SqlBackend`)
instance (MonadIO m) => AccountRepository (SqlPersistT m) where
  query ano = get (AccountKey ano)

  store acc = do
      _ <- insert_ acc
      return acc

  queryByOpenDate dt = do 
      es <- selectList [AccountOpenDate ==. dt] []
      return $ unEntity <$> es

  allAccounts = do
      es <- selectList [] []
      return $ unEntity <$> es

  upsert acc = do 
    a <- query $ acc ^. accountNo
    _ <- maybe (insert_ acc) (replace (AccountKey $ acc ^. accountNo)) a
    return acc