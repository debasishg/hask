{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Service.AccountService where

import           Data.Text
import           Data.Maybe
import           Control.Monad.Reader (MonadReader, liftIO)
import           Control.Monad.Validate
import qualified Money as Y
import           Control.Lens hiding (element)

import Account
import ValidateAeson
import Repository.AccountRepository
import Repository.SqliteAccountRepository
import Repository.AccountRepo
import           Repository.SqliteUtils
import           Control.Monad.Trans.Class      ( lift )
import Control.Monad.IO.Unlift (MonadIO (..), MonadUnliftIO)
import           Control.Monad.Logger (MonadLogger)

debtAccount :: forall m. (MonadReader Env m, 
                          MonadValidate [Error] m, 
                          MonadUnliftIO m,
                          MonadLogger m
                          ) => Y.Dense "USD" -> Text -> m Account
debtAccount amount accNo = 
  runSqliteAction2 (query accNo)
      >>= \ma -> updateBalance amount (fromJust ma)

debitAccount :: forall m. (MonadReader Env m, 
                           MonadValidate [Error] m, 
                           AccountRepository m) => Y.Dense "USD" -> Text -> m Account
debitAccount amount accNo = do 
  maybeAccount <- query accNo  
  maybe (refuteErr $ InvalidAccountNumber accNo) (updateBalance ((-1) * amount)) maybeAccount

-- debit :: forall m. (MonadReader Env m, 
                    -- MonadValidate [Error] m) => Y.Dense "USD" -> Account -> m Account
-- debit amount = updateBalance ((-1) * amount)

creditAccount :: forall m. (MonadReader Env m, 
                            MonadValidate [Error] m, 
                            AccountRepository m) => Y.Dense "USD" -> Text -> m Account
creditAccount amount accNo = do 
  maybeAccount <- query accNo  
  maybe (refuteErr $ InvalidAccountNumber accNo) (updateBalance amount) maybeAccount

transfer :: forall m. (MonadReader Env m, 
                       MonadValidate [Error] m, 
                       AccountRepository m) => Y.Dense "USD" -> Text -> Text -> m (Account, Account)
transfer amount from to = do
  f <- debitAccount amount from
  t <- creditAccount amount to
  return (f, t)