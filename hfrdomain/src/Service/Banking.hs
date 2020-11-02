{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}


module Service.Banking where

import qualified Money as Y
import qualified Data.Map as M
import Data.Text ( Text )
import Data.Foldable ( Foldable(foldl', null) )
import Data.Pool ( Pool )
import Data.Time ( UTCTime )
import Data.Maybe (fromJust)
import Validation (Validation (..) )
import Polysemy ( Sem, runM, Embed )
import Polysemy.Input ( Input, runInputConst )
import Control.Applicative ( Alternative((<|>)) )
import Control.Monad.Trans.Maybe ( MaybeT(MaybeT, runMaybeT) ) 
import Control.Monad.State ( MonadState(put, get), modify )
import Control.Lens ( (&), (^.), (%~) )
import Database.Persist.Sqlite (SqlBackend)
import Database.Redis (Connection)

import Model.Schema
    ( accountNo,
      accountType,
      currentBalance,
      transactionAccountNo,
      transactionAmount,
      transactionType,
      Account,
      MoneyUSD,
      Transaction )
import Model.Account (isAccountClosed, balanceInCurrency)
import Model.Transaction (makeTransaction)
import Model.TransactionType ( TransactionType(..) )
import Model.AccountType ( AccountType(Ch) )

import qualified Repository.AccountRepository as AR
import qualified Repository.TransactionRepository as TR
import qualified Repository.AccountCache as AC

-- | a domain service for doing debit or credit transactions
-- | each transaction created will be added to transaction table and
-- | balance in account table will be updated depending on the type
-- | of the transaction
transact :: Pool SqlBackend 
    -> Connection 
    -> UTCTime 
    -> TransactionType 
    -> Text 
    -> UTCTime 
    -> Y.Dense "USD" 
    -> IO ()
transact conn rconn utcCurrent txnType ano txnDate amount = runAllEffects conn rconn doTransaction
  where 
    doTransaction = do 

      maybeAcc <- getAccount 

      maybe (error $ "Invalid account " ++ show ano) doDB maybeAcc
        where
          doDB acc = case makeTransaction utcCurrent Dr txnDate amount ano of
            Success txn -> TR.store txn >> AR.upsert (updatedBalance acc)
            Failure err -> error $ show err

          updatedBalance acc = case txnType of
             Dr -> acc & currentBalance %~ subtract amount
             Cr -> acc & currentBalance %~ (+ amount)

          getAccount = runMaybeT $
                MaybeT (AC.fetchCachedAccount ano) 
            <|> MaybeT (AR.queryAccount ano)

-- | Transfer amount from one account to another
-- | This is a cached service where we try to fetch accounts from the cache first and if that
-- | fails we fall back to the database. Similarly after update to database we update the cache
-- | with the updated accounts
-- |
-- | The function offers transfer in any currency - hence you need to pass the exchange rate with
-- | USD since account maintains balance in USD. If you want to pass amount in USD, pass 1 as the
-- | exchange rate
transfer :: Pool SqlBackend 
    -> Connection 
    -> Text 
    -> Text 
    -> Y.Dense target
    -> Y.ExchangeRate "USD" target
    -> IO ()
transfer conn rconn fromAccountNo toAccountNo amount exchangeRateWithUSD = 
  runAllEffects conn rconn doTransfer
    where 
      doTransfer = updateAccountBalances >>= \accs -> do
        AR.upsertMany accs
        AC.cacheAccounts accs 
          where
            updateAccountBalances = 
              updateBalances <$> getAccount fromAccountNo <*> getAccount toAccountNo
                where
                  getAccount = \ano -> runMaybeT $ 
                        MaybeT (AR.queryAccount ano) 
                    <|> MaybeT (AR.queryAccount ano)

                  updateBalances (Just fa) (Just ta)  = [fa & balanceInCurrency exchangeRateWithUSD %~ subtract amount, 
                                                         ta & balanceInCurrency exchangeRateWithUSD %~ (+ amount)]

                  updateBalances (Just _) Nothing     = error $ "To account [" ++ show toAccountNo ++ "] does not exist"
                  updateBalances Nothing (Just _)     = error $ "From account [" ++ show fromAccountNo ++ "] does not exist"
                  updateBalances Nothing Nothing      = error $ "From account [" ++ show fromAccountNo ++ "] and To account [" ++ show toAccountNo ++ "] does not exist"

-- | a domain service that accesses multiple repositories to fetch account and
-- | transactions, computes the net value of all transactions fetched and updates
-- | the balance in the account repository
netValueTransactionsForAccount :: Pool SqlBackend -> Connection -> Text -> IO ()
netValueTransactionsForAccount conn rconn ano = runAllEffects conn rconn doNetValueComputation
  where 
    doNetValueComputation = do

      -- try to get the account from cache, if not found, fetch from database
      maybeAcc  <- getAccount

      -- fail if the account does not exist
      -- if exists, check if the account is already closed
      let maybeCloseDate = maybe (fail $ "Invalid account " ++ show ano) 
                                 isAccountClosed 
                                 maybeAcc

      -- fail if the account is already closed
      -- otherwise compute net value of all transactions for the account and update
      maybe (computeNetValueAndUpdate $ fromJust maybeAcc) 
            accountClosedErr 
            maybeCloseDate

        where
          getAccount = runMaybeT $
                MaybeT (AC.fetchCachedAccount ano) 
            <|> MaybeT (AR.queryAccount ano)

          accountClosedErr closeDate = error ("Account number " ++ show ano ++ " is closed on " ++ show closeDate)
          computeNetValueAndUpdate acc = do
            txns <- TR.queryByAccount ano

            let updated = updateBalance acc txns

            AR.store updated
            AC.cacheAccount updated

runAllEffects :: 
     Pool SqlBackend 
  -> Connection
  -> Sem '[AR.AccountRepository, TR.TransactionRepository, AC.AccountCache, Input (Pool SqlBackend), Input Connection, Embed IO] a 
  -> IO a

runAllEffects conn rconn program =
  program                    
    & AR.runAccountRepository
    & TR.runTransactionRepository
    & AC.runAccountCache
    & runInputConst conn  
    & runInputConst rconn  
    & runM

-- MonadState, as a typeclass, gives you the ability to write generically over all Monads with state.
-- The idea here is to maintain a mutable state as the tax profile that gets updated
-- as soon as the tax for an account is computed. Maybe this use case is a bit of an overkill,
-- but we are trying to play around with an abstraction of mutable state.
updateTaxProfile :: MonadState MoneyUSD m => [Account] -> m ()
updateTaxProfile accounts = do
  if | Data.Foldable.null accounts         -> return ()
     | isChecking $ Prelude.head accounts  -> updateTaxProfile (Prelude.tail accounts)
     | otherwise                           -> modify (+ taxOnBalance (Prelude.head accounts)) >> updateTaxProfile (Prelude.tail accounts)
         where
           taxOnBalance account = fromJust $ Y.dense $ (toRational $ (account ^. currentBalance)) * 0.1
           isChecking account = account ^. accountType == Ch

-- | Build the tax deduction profile of accounts for the list of transactions
-- | passed as input
buildTxnTaxProfile :: MonadState (M.Map Text MoneyUSD) m => [Transaction] -> m ()
buildTxnTaxProfile txns = do
  if | Data.Foldable.null txns         -> return ()
     | otherwise                       -> (do
         st <- get
         let txn = Prelude.head txns
             ano = txnAccountNo txn
         maybe 
           (put (M.insert ano (taxOn txn) st)) 
           (\_ -> (put (M.adjust (+ (taxOn txn)) ano st)))
           (M.lookup ano st)) >> buildTxnTaxProfile (Prelude.tail txns)
         where
           txnAccountNo txn = txn ^. transactionAccountNo
           taxOn txn = fromJust $ Y.dense $ (toRational $ (txn ^. transactionAmount)) * 0.1

-- | Build the tax deduction profile of accounts for the list of transactions
-- | passed as input
buildAccountTaxProfile :: MonadState (M.Map Text MoneyUSD) m => [Account] -> m ()
buildAccountTaxProfile accounts = do
  if | Data.Foldable.null accounts     -> return ()
     | otherwise                       -> (do
         st <- get
         let account = Prelude.head accounts
             ano = no account
         maybe 
           (put (M.insert ano (taxOn account) st)) 
           (\_ -> (put (M.adjust (+ (taxOn account)) ano st)))
           (M.lookup ano st)) >> buildAccountTaxProfile (Prelude.tail accounts)
         where
           no account = account ^. accountNo
           taxOn account = fromJust $ Y.dense $ (toRational $ (account ^. currentBalance)) * 0.1

-- | updates the current balance of an account from a 
-- | list of transactions
updateBalance :: Account -> [Transaction] -> Account
updateBalance account txns = 
  let amount = foldl' (+) zeroDollars (signValAmount <$> txns)
  in account & currentBalance %~ (+ amount)
    where 
      zeroDollars = 0 :: Y.Dense "USD"
      signValAmount txn = 
        if txn ^. transactionType == Cr
          then txn ^. transactionAmount
          else (-1) * (txn ^. transactionAmount)