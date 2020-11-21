module Lib.Service.Account
       ( accountByEmail ) where

import Lib.App (WithError)
import Lib.Core.Email (Email (..))
import Lib.Core.User (User (..))
import Lib.Core.Id ( Id(unId) ) 
import Lib.Core.Account (Account (..))
import Lib.Db.Functions (WithDb)
import Lib.Repository.UserRepo ( UserRepo(..) ) 
import Lib.Repository.AccountRepo
    ( AccountRepo(getAccountByUserId) ) 

accountByEmail :: (WithDb env m, WithError m, WithLog env m, AccountRepo m, UserRepo m) => Email -> m Account
accountByEmail email = do
  User {..} <- getUserByEmail email
  log D $ "Fetched user " <> userName <> " with id " <> unId userId <> " with email: " <> unEmail email
  getAccountByUserId $ unId userId
