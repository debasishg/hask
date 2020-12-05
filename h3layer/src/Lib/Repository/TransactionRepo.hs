module Lib.Repository.TransactionRepo where

import Lib.App (App)
import Lib.Core.Transaction (Transaction (..))
import Lib.Repository.Transaction (transactionsByAccountNo)

-- | typeclass for 'TransactionRepo'. The only assumption is that it's defined
-- in terms of a monad 'm'
class (Monad m) => TransactionRepo m where
  getTransactionsByAccountNo :: Text -> m [Transaction]

-- | an instance of the typeclass for the Application monad
-- that gets the implementations from a concrete postgresql based
-- implementation
instance TransactionRepo App where
  getTransactionsByAccountNo = transactionsByAccountNo
