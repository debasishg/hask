{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib.App.Env where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Colog (HasLog (..), Message)

type DbPool = Pool Connection

data Env (m :: Type -> Type) = Env
    { envDbPool        :: !DbPool
    , envLogAction     :: !(LogAction m Message)
    }

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