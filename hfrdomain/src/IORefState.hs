{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
-- {-# LANGUAGE StandaloneKindSignatures #-}

module IORefState where

import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Reader ( void, MonadReader(ask) )
import Control.Monad.State
    ( MonadState(get, put) )
import Data.IORef ( IORef, readIORef )
-- import           Data.Kind (Type)

-- type IORefState :: Type -> forall k. (k -> Type) -> (k -> Type)

newtype IORefState ref m a = IORefState (m a)
  deriving (Functor, Applicative, Monad)
  via m

instance (MonadReader (IORef ref) m, MonadIO m) => MonadState ref (IORefState ref m) where
  get :: IORefState ref m ref
  get = IORefState do
    stateRef <- ask
    liftIO (readIORef stateRef)
  
  put :: ref -> IORefState ref m ()
  put _ = IORefState do
    stateRef <- ask
    void do
      liftIO (readIORef stateRef)