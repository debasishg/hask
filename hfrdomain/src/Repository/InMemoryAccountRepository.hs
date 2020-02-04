{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module Repository.InMemoryAccountRepository where

import           Data.Text
import qualified Data.Map.Strict as M
import           Control.Monad.State (StateT, get, gets, put)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Control.Lens hiding (element)

import           Model.Account
import           Repository.AccountRepository

type AccountMap = M.Map Text Account 

newtype InMemoryAccountRepository a = InMemoryAccountRepository (StateT AccountMap IO a)
    deriving (Functor, Applicative, Monad)

instance MonadIO InMemoryAccountRepository where
    liftIO action = InMemoryAccountRepository $ liftIO action

instance AccountRepository InMemoryAccountRepository where
    query accNo = InMemoryAccountRepository $ 
        gets (M.lookup accNo) 

    store account = InMemoryAccountRepository $ do
        db <- get
        let db' = M.insert (account ^. accountNo) account db
        put db'
        return account

    queryByOpenDate dt = InMemoryAccountRepository $ 
        Prelude.filter (\a -> a ^. accountOpenDate == dt) <$> gets M.elems

    allAccounts = InMemoryAccountRepository $ 
        gets M.elems 
