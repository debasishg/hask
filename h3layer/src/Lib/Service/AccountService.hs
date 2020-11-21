module Lib.Service.AccountService where

import Lib.Core.Email (Email (..))
import Lib.Core.Account (Account (..))
import Lib.Core.Transaction (Transaction (..))
import Lib.Service.Account ( accountByEmail, transactionsByEmail ) 
import Lib.App (App)

class (Monad m) => AccountService m where
  getAccountByEmail :: Email -> m Account
  getTransactionsByEmail :: Email -> m [Transaction]

instance AccountService App where
  getAccountByEmail = accountByEmail
  getTransactionsByEmail = transactionsByEmail
