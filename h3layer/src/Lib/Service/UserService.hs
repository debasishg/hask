module Lib.Service.UserService where

import Lib.Core.Email (Email (..))

class (Monad m) => UserService m where
  getUserName :: Email -> m Text