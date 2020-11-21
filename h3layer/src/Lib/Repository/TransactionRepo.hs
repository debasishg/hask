module Lib.Repository.TransactionRepo where

import Lib.App (App)
import Lib.Core.Transaction (Transaction (..))
import Lib.Repository.Transaction (transactionsByAccountNo)

class (Monad m) => TransactionRepo m where
  getTransactionsByAccountNo :: Text -> m [Transaction]

instance TransactionRepo App where
  getTransactionsByAccountNo = transactionsByAccountNo
