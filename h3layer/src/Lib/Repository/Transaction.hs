{-# LANGUAGE QuasiQuotes #-}

-- | SQL queries to work with the @users@ table.

module Lib.Repository.Transaction
       ( transactionsByAccountNo ) where

import Lib.App (WithError)
import Lib.Core.Transaction (Transaction (..))
import Lib.Db.Functions (WithDb, queryNamed)

-- | concrete implementations based on postgresql that uses
-- mtl style constraints for effects

transactionsByAccountNo :: (WithDb env m, WithError m) => Text -> m [Transaction]
transactionsByAccountNo no = queryNamed [sql|
    SELECT id, amount, txn_date, account_no
    FROM transactions
    WHERE account_no = ?accountNo
|] [ "accountNo" =? no ]
