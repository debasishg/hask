{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @users@ table.

module Lib.Repository.Account
       ( accountByUserId, accountClosed ) where

import Data.Time ( UTCTime )
import Lib.App (WithError)
import Lib.Core.Account (Account (..))
import Lib.Core.Id (Id (..))
import Lib.Db.Functions (WithDb, asSingleRow, queryNamed)

accountByUserId :: (WithDb env m, WithError m) => Text -> m Account
accountByUserId uId = asSingleRow $ queryNamed [sql|
    SELECT no, name, open_date, close_date, user_id 
    FROM accounts
    WHERE user_id = ?(Id uId)
|] [ "user_id" =? Id uId ]

accountClosed :: (WithDb env m, WithError m, FromRow (Maybe UTCTime)) => Text -> m (Maybe UTCTime)
accountClosed no = asSingleRow $ queryNamed [sql|
    SELECT close_date
    FROM accounts
    WHERE no = ?no
|] [ "no" =? no ]