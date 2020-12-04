module Lib.Service.Account
       ( accountByEmail, transactionsByEmail ) where

import Lib.App (WithError)
import Lib.Core.Account (Account, getAccountNo)
import Lib.Core.Email (Email (..))
import Lib.Core.Id (Id (unId))
import Lib.Core.Transaction (Transaction (..))
import Lib.Core.User (User (..))
import Lib.Db.Functions (WithDb)
import Lib.Repository.AccountRepo (AccountRepo (getAccountByUserId))
import Lib.Repository.TransactionRepo (TransactionRepo (getTransactionsByAccountNo))
import Lib.Repository.UserRepo (UserRepo (..))

accountByEmail :: (WithDb env m, WithError m, WithLog env m, AccountRepo m, UserRepo m) => Email -> m Account
accountByEmail email = do
  User {..} <- getUserByEmail email
  log D $ "Fetched user " <> userName <> " with id " <> unId userId <> " with email: " <> unEmail email
  getAccountByUserId $ unId userId

transactionsByEmail :: (WithDb env m, WithError m, WithLog env m, AccountRepo m, UserRepo m, TransactionRepo m) => Email -> m [Transaction]
transactionsByEmail email = do
  a <- accountByEmail email
  getTransactionsByAccountNo (getAccountNo a)
