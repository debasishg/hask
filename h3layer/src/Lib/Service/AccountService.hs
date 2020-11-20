module Lib.Service.AccountService where

import Lib.Core.Email (Email (..))
import Lib.Core.Account (Account (..))
import Lib.Service.Account ( getAccountByEmail ) 
import Lib.App (App)

class (Monad m) => AccountService m where
  getAccountByEmail :: Email -> m Account

instance AccountService App where
  getAccountByEmail = Lib.Service.Account.getAccountByEmail
