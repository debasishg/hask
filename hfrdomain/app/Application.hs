{-# OPTIONS_GHC -foptimal-applicative-do #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Application where

import qualified Money as Y
import           Data.Foldable()
import           Data.Maybe                     (fromJust)
import           Data.Time
import           Data.Ratio                     ( (%) )
import           Control.Lens

import           Model.Account
import           Service.AccountService

-- | show how to run a bunch of actions for a bunch of accounts
main :: IO [Account]
main =   runMigrateActions 
     >>  openNewAccounts 
     >>= runAllActions
     where
       runAllActions accounts = do
         utcCurrent <- getCurrentTime  
         runActionsForAccounts [Credit (200 :: Y.Dense "USD"), 
                                Credit (400 :: Y.Dense "USD"),
                                Close utcCurrent,
                                Credit (400 :: Y.Dense "USD")] accounts 
         >>= persistAccounts

-- | show how to run a bunch of actions for an account
app :: IO Account
app =   runMigrateActions 
    >>  openNewAccounts 
    >>= persistAccounts
    >>  runAccountActions
    where
      runAccountActions = 
        runActionsForAccountNo [Credit (200 :: Y.Dense "USD"), Credit (400 :: Y.Dense "USD")] "0123456789" 
        >>= persistAccount

-- | shows how to fetch the balance of an account in a different currency
app2 :: IO ()
app2 =   runMigrateActions
     >>  openNewAccounts
     >>= persistAccounts
     >>  runAccountActions
     >>= print . view (balanceInCurrency $ fromJust usd2inr)
     where
       usd2inr = Y.exchangeRate (70 % 1) :: Maybe (Y.ExchangeRate "USD" "INR")
       runAccountActions =
         runActionsForAccountNo [Credit (200 :: Y.Dense "USD"), Credit (400 :: Y.Dense "USD")] "0123456789" 