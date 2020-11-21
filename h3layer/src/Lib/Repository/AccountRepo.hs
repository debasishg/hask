module Lib.Repository.AccountRepo where

import Data.Time (UTCTime)
import Lib.App (App)
import Lib.Core.Account (Account (..))
import Lib.Repository.Account (accountByUserId, accountClosed)

class (Monad m) => AccountRepo m where
  getAccountByUserId :: Text -> m Account
  isAccountClosed :: (FromRow (Maybe UTCTime)) => Text -> m (Maybe UTCTime)

instance AccountRepo App where
  getAccountByUserId = accountByUserId
  isAccountClosed = accountClosed
