{-# OPTIONS_GHC -foptimal-applicative-do #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Aeson.QQ (aesonQQ)
import Control.Monad.Validate (runValidateT, runValidate, refute)
import Control.Monad.Reader
import Data.Foldable (for_)
import BankAccount

main :: IO ()
main = do 
  let env = Env []
      testcase input = do
        accRdr <- runValidateT <$> makeBankAccount input
        return $ runReader accRdr env

  a <- testcase [aesonQQ| { "account_no": "1234567890", "account_name": "debasish", "account_open_date": "\"1984-10-15T00:00:00Z\"", "account_close_date": "\"1994-01-15T00:00:00Z\"", "account_current_balance": "[\"USD\",3200,1]" } |]
  b <- testcase [aesonQQ| { "account_no": "1234567890", "account_name": "debasish", "account_open_date": "\"1984-10-15T00:00:00Z\"", "account_close_date": "\"1994-01-15T00:00:00Z\"", "account_current_balance": "[\"USD\",32,1]" } |]
  c <- testcase [aesonQQ| { "account_no": "1234567890", "account_name": "debasish", "account_open_date": "\"1984-10-15T00:00:00Z\"", "account_close_date": "\"1984-01-15T00:00:00Z\"" } |]
  d <- testcase [aesonQQ| { "account_no": "1234567890", "account_name": "debasish", "account_open_date": "" } |]
  e <- testcase [aesonQQ| { "account_no": "12567890", "account_name": "debasish" } |]
  f <- testcase [aesonQQ| { "account_no": "125678", "account_name": "" } |]

  for_ [a, b, c, d, e, f] print 