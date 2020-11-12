{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @users@ table.

module Lib.Db.User
       ( getUserByEmail
       , getUser
       ) where

import Lib.App (WithError)
import Lib.Core.Email (Email (..))
import Lib.Core.User (User (..))
import Lib.Db.Functions (WithDb, asSingleRow, queryNamed)

getUserByEmail :: (WithDb env m, WithError m) => Email -> m User
getUserByEmail email = asSingleRow $ queryNamed [sql|
    SELECT id, email, name, pwd_hash
    FROM users
    WHERE email = LOWER(?email)
|] [ "email" =? email ]

getUser :: (WithDb env m, WithError m, WithLog env m) => Email -> m Text
getUser email = do
  User {..} <- getUserByEmail email
  log D $ "Fetched user " <> userName <> " with email: " <> unEmail email
  return userName
