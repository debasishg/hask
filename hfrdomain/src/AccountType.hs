{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}

module AccountType where

import qualified Database.Persist.TH as PTH
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data AccountType = Ch | Sv deriving (Show, Read, Generic)
instance FromJSON AccountType
instance ToJSON AccountType

PTH.derivePersistField "AccountType"