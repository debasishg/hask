module Service.TransactionService where

import           Control.Monad.Validate
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import           Data.Aeson (Value(..))

import           Model.Transaction
import           Errors

makeTransactionAggregateFromContext :: Value -> EitherT [Error] IO Transaction
makeTransactionAggregateFromContext jsonValue = 
  let env = Env []
      testcase input = do
        txnRdr <- runValidateT <$> makeTransaction input
        return $ runReader txnRdr env

  in EitherT $ testcase jsonValue
