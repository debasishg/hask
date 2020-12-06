{-# LANGUAGE OverloadedStrings #-}

module Lib ( main ) where

import           Data.Text (pack)
import           Data.Time (UTCTime, addUTCTime, getCurrentTime)
import           Lib.App (AppEnv, Env (..))
import           Lib.Config (Config (..), loadConfig)
import           Lib.Core.Account (Account, mkAccount)
import           Lib.Core.Email (Email (..))
import           Lib.Core.Transaction (MoneyUSD, Transaction (..), zeroDollars)
import           Lib.Db (initialisePool)
import           Lib.Effects.Log (mainLogAction, runAppLogIO)
import           Lib.Service (AccountService (..))
import qualified Relude.Unsafe as Unsafe
import           Validation (successes)


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

accounts :: UTCTime -> [Account]
accounts currentTime = do
  let a1 = mkAccount currentTime "a-01234567" "a-name-1" (addUTCTime (-3600) currentTime) Nothing (pack "id1")
  let a2 = mkAccount currentTime "a-12345678" "a-name-2" (addUTCTime (-3600) currentTime) Nothing (pack "id1")
  successes [a1, a2]

fetchTransactionAmounts :: (AccountService m) => Email -> m [MoneyUSD]
fetchTransactionAmounts = ((txnAmount <$>) `fmap`) . getTransactionsByEmail

computeBalance :: (AccountService m) => [MoneyUSD] -> m MoneyUSD
computeBalance amounts = return $ foldl' (+) zeroDollars amounts

mkApp :: AccountService m => UTCTime -> Email -> m MoneyUSD
mkApp currentTime email = do
    let acc = Unsafe.head $ accounts currentTime
    _ <- addNewAccount acc
    fetchTransactionAmounts email >>= computeBalance

main :: IO ()
main = do
    currentTime <- getCurrentTime
    config <- loadConfig
    env <- mkAppEnv config
    runAppLogIO env (mkApp currentTime (Email "test@test.com")) >>= print

-- main =
--     loadConfig >>=
--         mkAppEnv >>=
--             flip runAppLogIO (mkApp $ Email "test@test.com") >>=
--                 print
--
