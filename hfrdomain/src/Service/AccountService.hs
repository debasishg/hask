{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Service.AccountService where

import           Data.Text
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.Validate
import qualified Money as Y

import Account
import ValidateAeson
import Repository.AccountRepository

debitAccount :: forall m. (MonadReader Env m, 
                           MonadValidate [Error] m, 
                           AccountRepository m) => Y.Dense "USD" -> Text -> m Account
debitAccount amount accNo = do 
  maybeAccount <- query accNo  
  maybe (refuteErr $ InvalidAccountNumber accNo) (updateBalance ((-1) * amount)) maybeAccount

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