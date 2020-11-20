module Lib.Repository.UserRepo where

import Lib.Core.Email (Email (..))
import Lib.Core.User (User (..))
import Lib.Repository (getUserByEmail)
import Lib.App (App)

class (Monad m) => UserRepo m where
  getUserByEmail :: Email -> m User

instance UserRepo App where
  getUserByEmail = Lib.Repository.getUserByEmail
