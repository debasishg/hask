{-# LANGUAGE OverloadedStrings #-}

module Lib ( main ) where

import Lib.App (AppEnv, Env (..))
import Lib.Config (Config (..), loadConfig)
import Lib.Core.Email (Email (..))
import Lib.Core.Account (Account (..))
import Lib.Db (initialisePool)
import Lib.Service ( AccountService(..) ) 
import Lib.Effects.Log (mainLogAction, runAppLogIO)


mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config{..} = do
    envDbPool   <- initialisePool cDbCredentials
    let envLogAction = mainLogAction cLogSeverity
    pure Env{..}

mkApp :: (AccountService m) => Email -> m Account
mkApp = getAccountByEmail 

main :: IO ()
main = loadConfig >>= mkAppEnv >>= flip runAppLogIO (mkApp $ Email "test@test.com") >>= print
