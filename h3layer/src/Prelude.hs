{-# OPTIONS -fno-warn-orphans #-}

{-# LANGUAGE PatternSynonyms #-}

-- | Uses [relude](https://hackage.haskell.org/package/relude) as default Prelude.

module Prelude
       ( module Relude
       , module Colog
       , module Control.Lens
       , module Json
       , module Proto
       , module Sql

       , WithLog
       ) where

-- Reexport
import Relude

import Control.Lens ((.~), (^.))

import Colog (pattern D, pattern E, pattern I, LogAction (..), Severity (..), pattern W, log)

import Data.Aeson as Json (FromJSON (parseJSON), ToJSON (toJSON))

import Data.ProtoLens.Message as Proto (defMessage)

import Database.PostgreSQL.Simple.FromField as Sql (FromField (fromField))
import Database.PostgreSQL.Simple.FromRow as Sql (FromRow (fromRow), field)
import Database.PostgreSQL.Simple.SqlQQ as Sql (sql)
import Database.PostgreSQL.Simple.ToField as Sql (ToField (toField))
import Database.PostgreSQL.Simple.ToRow as Sql (ToRow (toRow))
import Database.PostgreSQL.Simple.Types as Sql (Only (..))
import PgNamed as Sql ((=?))

-- Internal
import qualified Colog (Message, WithLog)


-- | 'Colog.WithLog' alias specialized to 'Message' data type.
type WithLog env m = Colog.WithLog env Colog.Message m