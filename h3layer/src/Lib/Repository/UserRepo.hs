module Lib.Repository.UserRepo where

import Lib.Core.Email (Email (..))
import Lib.Core.User (User (..))

class (Monad m) => UserRepo m where
  getUserByEmail :: Email -> m User