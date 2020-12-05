{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @users@ table.

module Lib.Repository.User
       ( userByEmail ) where

import Lib.App (WithError)
import Lib.Core.Email (Email (..))
import Lib.Core.User (User (..))
import Lib.Db.Functions (WithDb, asSingleRow, queryNamed)

-- | concrete implementations based on postgresql that uses
-- mtl style constraints for effects

userByEmail :: (WithDb env m, WithError m) => Email -> m User
userByEmail email = asSingleRow $ queryNamed [sql|
    SELECT id, name, email, pwd_hash
    FROM users
    WHERE email = LOWER(?email)
|] [ "email" =? email ]
