{-# LANGUAGE DeriveAnyClass #-}

module Lib.Core.Account
       ( Account (..)
       ) where

import Data.Time ( UTCTime )
import Lib.Core.Id (Id)
import Lib.Core.User ( User )


-- | Data type representing row in the @users@ table.
data Account = Account
    { accountNo    :: !Text
    , accountName  :: !Text
    , openDate     :: !UTCTime 
    , closeDate    :: !(Maybe UTCTime)
    , userId       :: !(Id User)
    } deriving stock (Generic, Show, Eq)
      deriving anyclass (FromRow)
      deriving (FromJSON, ToJSON) via Account