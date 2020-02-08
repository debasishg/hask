{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}

module Model.TransactionType where

import qualified Database.Persist.TH as PTH
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

data TransactionType = Dr | Cr deriving (Show, Read, Generic)
instance FromJSON TransactionType
instance ToJSON TransactionType

PTH.derivePersistField "TransactionType"