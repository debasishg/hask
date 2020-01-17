{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module PersistentMoney where

import qualified Data.Text as T
import qualified Money as Y

import Database.Persist
import Database.Persist.Sql

-- |Typeclass instances for Money, which is not directly supported in persistent
instance PersistField (Y.Dense currency) where
  toPersistValue m = PersistText (Y.denseToDecimal Y.defaultDecimalConf Y.Round m)
  fromPersistValue (PersistText t) =
    case Y.denseFromDecimal Y.defaultDecimalConf t of  
      Just x -> Right x
      Nothing -> Left "Invalid Money"
  fromPersistValue x = Left $ "When trying to deserialize a Money: expected PersistText, received: " <> T.pack (show x)

instance PersistFieldSql (Y.Dense currency) where
  sqlType _ = SqlOther "varchar"
