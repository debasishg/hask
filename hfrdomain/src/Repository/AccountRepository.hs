{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}


module Repository.AccountRepository where

import Data.Text
import Data.Time
import           Control.Monad.Validate
import           Control.Monad.Trans.Class      ( lift )
import           Control.Monad.Trans.Reader
import           Control.Monad.Identity
import           Data.Functor.Identity

import Account

-- | Repository abstraction that's independent of the underlying database 
-- representation
class (Monad m) => AccountRepository m where
    query            :: Text                     -- ^ query by account number
                     -> m (Maybe Account)        -- ^ the fetched account, if found
    store            :: Account                  -- ^ store an account
                     -> m Account                
    queryByOpenDate  :: UTCTime                  -- ^ query by account open date
                     -> m [Account]              -- ^ the fetched account list, (maybe empty)
    allAccounts      :: m [Account]              -- ^ fetch all accounts

instance (AccountRepository m) => AccountRepository (ValidateT e m) where
  query = lift . query
  store = lift . store
  queryByOpenDate = lift . queryByOpenDate
  allAccounts = lift allAccounts
  {-# INLINE query #-}
  {-# INLINE store #-}
  {-# INLINE queryByOpenDate #-}
  {-# INLINE allAccounts #-}

instance {-# OVERLAPPABLE #-} (AccountRepository m) => AccountRepository (ReaderT Env m) where
  query = lift . query
  store = lift . store
  queryByOpenDate = lift . queryByOpenDate
  allAccounts = lift allAccounts
  {-# INLINE query #-}
  {-# INLINE store #-}
  {-# INLINE queryByOpenDate #-}
  {-# INLINE allAccounts #-}

instance AccountRepository Identity where
  query = query 
  store = store
  queryByOpenDate = queryByOpenDate
  allAccounts = allAccounts
  {-# INLINE query #-}
  {-# INLINE store #-}
  {-# INLINE queryByOpenDate #-}
  {-# INLINE allAccounts #-}