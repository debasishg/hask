{-# LANGUAGE OverloadedStrings #-}

module Lib ( main ) where

import Lib.App (AppEnv, Env (..))
import Lib.Config (Config (..), loadConfig)
import Lib.Core.Account (Account (..))
import Lib.Core.Email (Email (..))
import Lib.Db (initialisePool)
import Lib.Effects.Log (mainLogAction, runAppLogIO)
import Lib.Service (AccountService (..))


mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config{..} = do
    envDbPool   <- initialisePool cDbCredentials
    let envLogAction = mainLogAction cLogSeverity
    pure Env{..}

mkApp :: (AccountService m) => Email -> m Account
mkApp = getAccountByEmail

main :: IO ()
main = loadConfig >>= mkAppEnv >>= flip runAppLogIO (mkApp $ Email "test@test.com") >>= print
