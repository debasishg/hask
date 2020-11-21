{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lib.Core.Transaction
       ( Transaction (..)
       ) where

import           Data.Time (UTCTime)
import           Lib.Core.Id (Id)
import qualified Money as Y

type MoneyUSD = (Y.Dense "USD")

instance FromField MoneyUSD where
  fromField _ mdata =
    return money
    where
      money = case mdata of
        Just t -> case Y.denseFromDecimal Y.defaultDecimalConf (decodeUtf8 t) of
          Just x  -> x
          Nothing -> error "Invalid money"
        _      -> error "Invalid money"

-- | Data type representing row in the @transactions@ table.
data Transaction = Transaction
    { txnId     :: !(Id Transaction)
    , txnAmount :: !MoneyUSD
    , txnDate   :: !UTCTime
    , accountNo :: !Text
    } deriving stock (Generic, Show, Eq)
      deriving anyclass (FromRow)
      deriving (FromJSON, ToJSON) via Transaction
