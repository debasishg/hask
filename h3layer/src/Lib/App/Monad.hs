module Lib.App.Monad
       ( -- * Application monad
         App (..)
       , AppEnv
       , runAppAsIO
       ) where

import Control.Exception (catch, throwIO, try)
import Control.Monad.Except (MonadError (..))
import Relude.Extra.Bifunctor (firstF)

import Lib.App.Env (Env)
import Lib.App.Error (AppError, AppException (..))


-- | 'Env' data type parameterized by 'App' monad
type AppEnv = Env App

-- | Main application monad.
-- Notes below from Matt Parson's book Production Haskell
--
-- One reason for using this newtype is to prevent using naked IO as part of your
-- application monad. An option could be a type synonym 'type App = IO'. However, this doesn’t 
-- prevent us from using IO directly. If we ever modify the 'App' type, then we’ll need to modify 
-- each direct use of 'IO'. We can preempt this by writing a newtype and deriving 'MonadIO'. 
--
-- @
-- import Control.Monad.IO.Class (MonadIO(..))

-- newtype App a = App { unApp :: IO a }

-- instance MonadIO App where 
--    liftIO ioAction = 
--        App ioAction
-- @
--
-- or, with GeneralizedNewtypeDeriving: 
--
-- @
-- newtype App a = App { unApp :: IO a } 
--   deriving newtype (Functor, Applicative, Monad, MonadIO)
-- @
-- 
-- A few benefits out of this:
-- a. Forward compatibility. Anticipate wanting to change away from IO as the main type of our application
-- b. Opt-in for libraries. This technique makes it possible to write custom instances 
--    for type classes on your application type. Have a look at instances for 'AccountRepo' for example.

newtype App a = App
    { unApp :: ReaderT AppEnv IO a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)

instance MonadError AppError App where
    throwError :: AppError -> App a
    throwError = liftIO . throwIO . AppException
    {-# INLINE throwError #-}

    catchError :: App a -> (AppError -> App a) -> App a
    catchError action handler = App $ ReaderT $ \env -> do
        let ioAction = runApp env action
        ioAction `catch` \(AppException e) -> runApp env $ handler e
    {-# INLINE catchError #-}

{- | Helper for running route handlers in IO. Catches exception of type
'AppException' and unwraps 'AppError' from it.

Do not use this function to run the application. Use runners with logging from
"Lib.Effects.Log" module to also log the error.
-}
runAppAsIO :: AppEnv -> App a -> IO (Either AppError a)
runAppAsIO env = firstF unAppException . try . runApp env

{- | Helper for running 'App'.

Do not use this function to run the application. Use runners with logging from
"Lib.Effects.Log" module to also log the error.
-}
runApp :: AppEnv -> App a -> IO a
runApp env = usingReaderT env . unApp

-- runApp :: AppEnv -> App a -> IO a 
-- runApp appEnv action = runReaderT (unApp action) appEnv