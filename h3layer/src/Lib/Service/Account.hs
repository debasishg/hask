module Lib.Service.Account
       ( getAccountByEmail ) where

import Lib.App (WithError)
import Lib.Core.Email (Email (..))
import Lib.Core.User (User (..))
import Lib.Core.Id ( Id(unId) ) 
import Lib.Core.Account (Account (..))
import Lib.Db.Functions (WithDb)
import Lib.Repository.User ( getUserByEmail )
import Lib.Repository.AccountRepo
    ( AccountRepo(getAccountByUserId) ) 

getAccountByEmail :: (WithDb env m, WithError m, WithLog env m, AccountRepo m) => Email -> m Account
getAccountByEmail email = do
  User {..} <- getUserByEmail email
  log D $ "Fetched user " <> userName <> " with id " <> unId userId <> " with email: " <> unEmail email
  getAccountByUserId $ unId userId
