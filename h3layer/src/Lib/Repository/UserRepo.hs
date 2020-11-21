module Lib.Repository.UserRepo where

import Lib.Core.Email (Email (..))
import Lib.Core.User (User (..))
import Lib.Repository.User (userByEmail)
import Lib.App (App)

class (Monad m) => UserRepo m where
  getUserByEmail :: Email -> m User

instance UserRepo App where
  getUserByEmail = userByEmail
