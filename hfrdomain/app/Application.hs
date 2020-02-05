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
import           Data.Time

import           Model.Account
import           Service.AccountService

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

app :: IO Account
app =    runMigrateActions 
     >>  openNewAccounts 
     >>= persistAccounts
     >>  runAllAccountActions
     where
       runAllAccountActions = 
         runActionsForAccountNo [Credit (200 :: Y.Dense "USD"), Credit (400 :: Y.Dense "USD")] "0123456789" 
         >>= persistAccount