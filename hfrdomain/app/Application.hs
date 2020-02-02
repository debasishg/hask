{-# OPTIONS_GHC -foptimal-applicative-do #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Application where

import qualified Money as Y
import           Data.Foldable()

import           Account
import           Service.AccountService

main :: IO [Account]
main = runMigrateActions 
  >>  openNewAccounts 
  >>= runTransactionsForAccounts [Credit (200 :: Y.Dense "USD"), Credit (400 :: Y.Dense "USD")] 
  >>= persistAccounts