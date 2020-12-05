{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @accounts@ table.

module Lib.Repository.Account
       ( accountByUserId, accountClosed ) where

import Data.Time (UTCTime)
import Lib.App (WithError)
import Lib.Core.Account (Account, getCloseDate)
import Lib.Db.Functions (WithDb, asSingleRow, queryNamed)

-- | concrete implementations based on postgresql that uses
-- mtl style constraints for effects

accountByUserId :: (WithDb env m, WithError m) => Text -> m Account
accountByUserId userId = asSingleRow $ queryNamed [sql|
    SELECT no, name, open_date, close_date, user_id
    FROM accounts
    WHERE user_id = ?userId
|] [ "userId" =? userId ]

accountClosed :: (WithDb env m, WithError m) => Text -> m (Maybe UTCTime)
accountClosed no = asSingleRow
    (queryNamed
        [sql|
            SELECT no, name, open_date, close_date, user_id
            FROM accounts
            WHERE no = ?no
        |]
        ["no" =? no])
    <&> getCloseDate
