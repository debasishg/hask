{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib.App.Env
       ( Env (..)
       , Has (..)
       , grab

         -- * Type aliases for 'Env' fields
       , DbPool
       )  where

import Colog (HasLog (..), Message)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)

type DbPool = Pool Connection

-- | The environment of the application
-- This gives a name to the most common aspects of the application and centralizes the 
-- configuration aspects that need to be wired with the application. This also gives us 
-- the ability to write Haddock documentation for the environment. 

data Env (m :: Type -> Type) = Env
    { envDbPool    :: !DbPool
    , envLogAction :: !(LogAction m Message)
    }

-- class HasLog env msg m where
--     getLogAction :: env -> LogAction m msg
--     setLogAction :: LogAction m msg -> env -> env
--
-- instance HasLog (LogAction m msg) msg m where
--     getLogAction = id
--     setLogAction = const

instance HasLog (Env m) Message m where
    getLogAction :: Env m -> LogAction m Message
    getLogAction = envLogAction
    {-# INLINE getLogAction #-}

    setLogAction :: LogAction m Message -> Env m -> Env m
    setLogAction newAction env = env { envLogAction = newAction }
    {-# INLINE setLogAction #-}

class Has field env where
    obtain :: env -> field

instance Has DbPool                (Env m) where obtain = envDbPool
instance Has (LogAction m Message) (Env m) where obtain = envLogAction

grab :: forall field env m . (MonadReader env m, Has field env) => m field
grab = asks $ obtain @field
{-# INLINE grab #-}
