{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib
       ( mkAppEnv
       , main
       ) where

import qualified Lib.Repository.UserRepo as UserRepo
import qualified Lib.Service.UserService as UserService

import Lib.App (App, AppEnv, Env (..), runApp)
import Lib.Config (Config (..), loadConfig)
import Lib.Core.Email (Email (..))
import Lib.Db (getUser, getUserByEmail, initialisePool)
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

main :: IO ()
main = loadConfig >>= mkAppEnv >>= flip runApp (getUser $ Email "dghosh@acm.org") >>= print
