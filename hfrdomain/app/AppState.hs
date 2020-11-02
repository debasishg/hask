{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeInType, AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module AppState where

import qualified Data.Map as M
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Reader
    ( MonadReader(ask), ReaderT(ReaderT) )
import Control.Monad.State ( MonadState(get, put) )
import Data.IORef ( IORef, readIORef, writeIORef )
import Data.Text ( Text )

import Model.Schema ( MoneyUSD )

-- We define a type that abstracts an application state. We don't use
-- the `State` monad and instead use an `IORef`. Note `IORef` is safe
-- to use for single threaded use cases when we need some mutable state 
-- or when we want a mutable field inside a larger structure that is in 
-- turn held by a synchronization variable.
-- We need to use `MVar` or `STM` for more robust concurrency semantics
-- type AppState :: Type -> Type
newtype AppState s a = AppState { runApp :: IORef s -> IO a }
  deriving (
    Functor, Applicative, Monad,

    -- `MonadIO` gives us `liftIO :: IO a -> AppState s a`.
    -- Having `liftIO` essentially means our custom `App` type
    -- gets to inherit all of the built-in `IO` operations.
    MonadIO,

    -- `MonadReader (IORef s)` gives us `ask :: AppState (IORef s)`
    -- `ask` allows us to get an `IORef s` inside an `AppState` do block
    -- any time we want, deterministically (i.e., we'll get the same
    -- one every time we ask).
    MonadReader (IORef s)

    -- We're able to derive all of these wonderful instances because
    -- these instances already exist for `ReaderT (IORef s) IO` and
    -- because `AppState a` and `ReaderT (IORef s) IO a` have identical
    -- underlying implementations, namely `IORef s -> IO a`.
    ) via ReaderT (IORef s) IO

  -- deriving (MonadState s)
  -- via IORefState s @Type (AppState s)


-- We need to implement `MonadState MoneyUSD (AppState MoneyUSD)` so that we can
-- use `AppState` in place of the abstract `m`.
instance MonadState MoneyUSD (AppState MoneyUSD) where
  get :: AppState MoneyUSD MoneyUSD
  get = do
    stateRef <- ask                             -- ask for the state ref
    currentState <- liftIO (readIORef stateRef) -- read the current state
    return currentState                         -- return the current state

  put :: MoneyUSD -> AppState MoneyUSD ()
  put newState = do
    stateRef <- ask                       -- ask for the state ref
    liftIO (writeIORef stateRef newState) -- write the new state
    return ()                             -- return nothin'

instance MonadState (M.Map Text MoneyUSD) (AppState (M.Map Text MoneyUSD)) where
  get :: AppState (M.Map Text MoneyUSD) (M.Map Text MoneyUSD)
  get = do
    stateRef <- ask                             -- ask for the state ref
    currentState <- liftIO (readIORef stateRef) -- read the current state
    return currentState                         -- return the current state

  put :: M.Map Text MoneyUSD -> AppState (M.Map Text MoneyUSD) ()
  put newState = do
    stateRef <- ask                       -- ask for the state ref
    liftIO (writeIORef stateRef newState) -- write the new state
    return ()                             -- return nothin'
