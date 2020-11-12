{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib ( main ) where

import qualified Lib.Repository.UserRepo as UserRepo
import qualified Lib.Service.UserService as UserService

import Lib.App (App, AppEnv, Env (..), runApp, WithError)
import Lib.Config (Config (..), loadConfig)
import Lib.Core.Email (Email (..))
import Lib.Db (getUser, getUserByEmail, initialisePool, WithDb)
import Lib.Effects.Log (mainLogAction)


instance UserRepo.UserRepo App where
  getUserByEmail = Lib.Db.getUserByEmail

instance UserService.UserService App where
  getUser = Lib.Db.getUser

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config{..} = do
    -- IO configuration
    envDbPool   <- initialisePool cDbCredentials
    -- pure configuration
    let envLogAction = mainLogAction cLogSeverity
    pure Env{..}

mkApp :: (WithDb env m, WithError m, WithLog env m) => Email -> m Text
mkApp = getUser 

main :: IO ()
main = loadConfig >>= mkAppEnv >>= flip runApp (mkApp $ Email "dghosh@acm.org") >>= print
