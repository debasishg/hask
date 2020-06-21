{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}

module Model.AccountType where

import qualified Database.Persist.TH as PTH
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data AccountType = Ch | Sv deriving (Eq, Show, Read, Generic)
instance FromJSON AccountType
instance ToJSON AccountType

PTH.derivePersistField "AccountType"