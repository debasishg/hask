{-# OPTIONS_GHC -foptimal-applicative-do #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Time
import Data.Aeson.QQ (aesonQQ)
import Control.Monad.Validate (runValidateT, runValidate)
import Control.Monad.Reader
import Data.Foldable (for_)
import qualified Money as Y
import Account

main :: IO ()
main = do 
  let env = Env []
      testcase input = do
        accRdr <- runValidateT <$> makeAccount input
        return $ runReader accRdr env

  c <- getCurrentTime
  a <- testcase [aesonQQ| { "account_no": "1234567890", "account_type": "\"Sv\"", "account_name": "debasish", "account_open_date": "\"1984-10-15T00:00:00Z\"", "account_close_date": "", "account_current_balance": "[\"USD\",3200,1]", "rate_of_interest": 2.5 } |]

  case a of
    Left err  -> print $ show err
    Right acc -> print $ 
                   runValidate $ 
                     updateBalance acc (300 :: Y.Dense "USD") >>= 
                       flip updateBalance (-200 :: Y.Dense "USD") >>= 
                         flip close c