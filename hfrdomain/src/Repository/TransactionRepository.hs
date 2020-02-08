{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Repository.TransactionRepository (TransactionRepository (..)) where 

import Data.Int (Int64)
import Data.Text
import Data.Time
import Model.Transaction

-- | Repository abstraction that's independent of the underlying database 
-- representation
class (Monad m) => TransactionRepository m where
    query                                :: Text                     -- ^ query by account number
                                         -> m [Transaction]          -- ^ the fetched list of transactions for the account
    store                                :: Transaction              -- ^ store a transaction
                                         -> m Int64                
    queryByTransactionDate               :: UTCTime                  -- ^ query by transaction date
                                         -> m [Transaction]          -- ^ the fetched transaction list
    queryByAccountNTransactionDateRange  :: Text                     -- ^ query by account number and
                                         -> UTCTime                  -- ^ transaction date range
                                         -> UTCTime
                                         -> m [Transaction]          -- ^ the fetched transaction list
