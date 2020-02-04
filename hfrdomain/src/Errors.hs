{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}

module Errors where

import Data.Text
import Data.Aeson (Value(..))
import Data.Time

newtype Env
    = Env {envPath :: [Text]}
    deriving (Show, Eq)

data Error = Error { errPath :: [Text], errInfo :: ErrorInfo }
    deriving (Show, Eq)
data ErrorInfo
    = JSONBadValue Text Value
    | JSONMissingKey Text
    | InvalidDateValueInJSON Text
    | InvalidMoneyValueInJSON Text
    | InvalidAccountNumber Text 
    | InvalidAccountName Text
    | InvalidAccountOpenDate Text
    | InvalidAccountOpenCloseDateCombination Text
    | AccountBalanceLessThanMinimumBalance Text
    | AccountAlreadyClosed Text UTCTime
    | InsufficientFundsInAccount Text
    | InvalidAccountType Text
    | RateNotApplicableForCheckingAccount
    deriving (Show, Eq)

