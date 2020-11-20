module Lib.Service.UserService where

import Lib.Core.Email (Email (..))
import Lib.Service (getUserName)
import Lib.App (App)

class (Monad m) => UserService m where
  getUserName :: Email -> m Text

instance UserService App where
  getUserName = Lib.Service.getUserName
