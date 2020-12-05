{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @accounts@ table.

module Lib.Repository.Account
       ( accountByUserId
       , accountClosed
       , addAccount
       ) where

import Data.Time (UTCTime)
import Lib.App (WithError)
import Lib.Core.Account
    ( Account,
      getCloseDate,
      getUserId,
      getAccountNo,
      getAccountName,
      getOpenDate ) 
import Lib.Db.Functions
    ( asSingleRow, executeNamed, queryNamed, WithDb ) 

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

addAccount :: (WithDb env m, WithError m) => Account -> m Int64
addAccount account = 
    executeNamed
        [sql|
            INSERT INTO accounts 
            (no, name, open_date, close_date, user_id)]
            VALUES
            (?ano, ?anm, ?odt, ?cdt, ?uid)
        |]
        [ "ano" =? getAccountNo account
        , "anm" =? getAccountName account
        , "odt" =? getOpenDate account
        , "cdt" =? getCloseDate account
        , "uid" =? getUserId account
        ]
