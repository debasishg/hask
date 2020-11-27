{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib.Repository.AccountRepo where

import Data.Time (UTCTime)
import qualified Control.Monad.State.Strict as State
import qualified Relude.Unsafe as Unsafe

import Lib.App (App)
import Lib.Core.Id (Id (unId))
import Lib.Core.Account (Account (..))
import Lib.Repository.Account (accountByUserId, accountClosed)

class (Monad m) => AccountRepo m where
  getAccountByUserId :: Text -> m Account
  isAccountClosed :: Text -> m (Maybe UTCTime)

instance AccountRepo App where
  getAccountByUserId = accountByUserId
  isAccountClosed = accountClosed

instance Monad m => AccountRepo (State.StateT [Account] m) where
  getAccountByUserId uid = 
    StateT $ \s -> 
      return (Unsafe.head (filter(\a -> unId (userId a) == uid) s), s)

  isAccountClosed ano = 
    StateT $ \s -> 
      return (closeDate (Unsafe.head (filter (\a -> accountNo a == ano) s)), s)