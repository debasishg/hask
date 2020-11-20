{-# LANGUAGE OverloadedStrings #-}

module Lib ( main ) where

import Lib.App (AppEnv, Env (..), WithError)
import Lib.Config (Config (..), loadConfig)
import Lib.Core.Email (Email (..))
import Lib.Db (initialisePool, WithDb)
import Lib.Service (getUserName)
import Lib.Effects.Log (mainLogAction, runAppLogIO_)


mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config{..} = do
    -- IO configuration
    envDbPool   <- initialisePool cDbCredentials
    -- pure configuration
    let envLogAction = mainLogAction cLogSeverity
    pure Env{..}

mkApp :: (WithDb env m, WithError m, WithLog env m) => Email -> m Text
mkApp = getUserName 

main :: IO ()
main = loadConfig >>= mkAppEnv >>= flip runAppLogIO_ (mkApp $ Email "dghosh@acm.org") >>= print
