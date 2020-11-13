
-- | SQL queries to work with the @users@ table.

module Lib.Service.User
       ( getUserName ) where

import Lib.App (WithError)
import Lib.Core.Email (Email (..))
import Lib.Core.User (User (..))
import Lib.Db.Functions (WithDb)
import Lib.Repository.User

getUserName :: (WithDb env m, WithError m, WithLog env m) => Email -> m Text
getUserName email = do
  User {..} <- getUserByEmail email
  log D $ "Fetched user " <> userName <> " with email: " <> unEmail email
  return userName
