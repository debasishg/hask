{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib ( main ) where

import qualified Lib.Repository.UserRepo as UserRepo
import qualified Lib.Service.UserService as UserService

import Lib.App (App, AppEnv, Env (..), WithError)
import Lib.Config (Config (..), loadConfig)
import Lib.Core.Email (Email (..))
import Lib.Db (initialisePool, WithDb)
import Lib.Repository (getUserByEmail)
import Lib.Service (getUserName)
import Lib.Effects.Log (mainLogAction, runAppLogIO_)


instance UserRepo.UserRepo App where
  getUserByEmail = Lib.Repository.getUserByEmail

instance UserService.UserService App where
  getUserName = Lib.Service.getUserName

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
