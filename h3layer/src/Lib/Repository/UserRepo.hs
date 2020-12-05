module Lib.Repository.UserRepo where

import Lib.App (App)
import Lib.Core.Email (Email (..))
import Lib.Core.User (User (..))
import Lib.Repository.User (userByEmail)

-- | typeclass for 'UserRepo'. The only assumption is that it's defined
-- in terms of a monad 'm'
class (Monad m) => UserRepo m where
  getUserByEmail :: Email -> m User

-- | an instance of the typeclass for the Application monad
-- that gets the implementations from a concrete postgresql based
-- implementation
instance UserRepo App where
  getUserByEmail = userByEmail
