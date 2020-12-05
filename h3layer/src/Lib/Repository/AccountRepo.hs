{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Lib.Repository.AccountRepo where

import qualified Control.Monad.State.Strict as State
import           Data.Time (UTCTime)
import qualified Relude.Unsafe as Unsafe

import Lib.App (App)
import Lib.Core.Account (Account, getAccountNo, getCloseDate, getUserId)
import Lib.Core.Id (Id (unId))
import Lib.Repository.Account
    ( accountByUserId, accountClosed, addAccount ) 

-- | typeclass for 'AccountRepo'. The only assumption is that it's defined
-- in terms of a monad 'm'
class (Monad m) => AccountRepo m where
  getAccountByUserId :: Text -> m Account
  isAccountClosed :: Text -> m (Maybe UTCTime)
  insertAccount :: Account -> m Int64

-- | an instance of the typeclass for the Application monad
-- that gets the implementations from a concrete postgresql based
-- implementation
instance AccountRepo App where
  getAccountByUserId = accountByUserId
  isAccountClosed = accountClosed
  insertAccount = addAccount

-- | Mock instance of the typeclass for testing that uses StateT
instance Monad m => AccountRepo (State.StateT [Account] m) where
  getAccountByUserId uid =
    StateT $ \s ->
      return (Unsafe.head (filter(\a -> unId (getUserId a) == uid) s), s)

  isAccountClosed ano =
    StateT $ \s ->
      return (getCloseDate (Unsafe.head (filter (\a -> getAccountNo a == ano) s)), s)

  insertAccount acc =
    StateT $ \s -> 
      return (1::Int64, s ++ [acc])
