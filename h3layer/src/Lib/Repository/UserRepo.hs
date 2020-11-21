module Lib.Repository.UserRepo where

import Lib.App (App)
import Lib.Core.Email (Email (..))
import Lib.Core.User (User (..))
import Lib.Repository.User (userByEmail)

class (Monad m) => UserRepo m where
  getUserByEmail :: Email -> m User

instance UserRepo App where
  getUserByEmail = userByEmail
