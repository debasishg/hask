module Lib.Service.AccountService where

import Lib.App (App)
import Lib.Core.Account (Account)
import Lib.Core.Email (Email (..))
import Lib.Core.Transaction (Transaction (..))
import Lib.Service.Account (accountByEmail, addAccount, transactionsByEmail)

class (Monad m) => AccountService m where
  getAccountByEmail :: Email -> m Account
  getTransactionsByEmail :: Email -> m [Transaction]
  addNewAccount :: Account -> m Int64

instance AccountService App where
  getAccountByEmail = accountByEmail
  getTransactionsByEmail = transactionsByEmail
  addNewAccount = addAccount
