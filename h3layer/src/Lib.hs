{-# LANGUAGE OverloadedStrings #-}

module Lib ( main ) where

import Lib.App (AppEnv, Env (..))
import Lib.Config (Config (..), loadConfig)
import Lib.Core.Email (Email (..))
import Lib.Core.Transaction (MoneyUSD, Transaction (..), zeroDollars)
import Lib.Db (initialisePool)
import Lib.Effects.Log (mainLogAction, runAppLogIO)
import Lib.Service (AccountService (..))


mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config{..} = do
    envDbPool   <- initialisePool cDbCredentials
    let envLogAction = mainLogAction cLogSeverity
    pure Env{..}

-- mkApp email = do
    -- txns <- getTransactionsByEmail email
    -- return $ accountNo <$> txns

-- mkApp email = getTransactionsByEmail email >>= \txns -> return $ accountNo <$> txns
-- mkApp = ((accountNo <$>) `fmap`) . getTransactionsByEmail

fetchTransactionAmounts :: (AccountService m) => Email -> m [MoneyUSD]
fetchTransactionAmounts = ((txnAmount <$>) `fmap`) . getTransactionsByEmail

computeBalance :: (AccountService m) => [MoneyUSD] -> m MoneyUSD
computeBalance amounts = return $ foldl' (+) zeroDollars amounts

mkApp :: AccountService m => Email -> m MoneyUSD
mkApp email = fetchTransactionAmounts email >>= computeBalance

main :: IO ()
main =
    loadConfig >>=
        mkAppEnv >>=
            flip runAppLogIO (mkApp $ Email "test@test.com") >>=
                print
