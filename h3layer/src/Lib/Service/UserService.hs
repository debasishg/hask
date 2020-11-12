module Lib.Service.UserService where

import Lib.Core.Email (Email (..))

class (Monad m) => UserService m where
  getUser :: Email -> m Text