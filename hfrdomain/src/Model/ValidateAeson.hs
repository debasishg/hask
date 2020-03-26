{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}

module Model.ValidateAeson where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Money as Y
import qualified Data.HashMap.Strict as M

import           Data.Text
import           Data.Aeson (Value(..), decode, Object)
import           Data.Time
import           Data.Text.Encoding (encodeUtf8)
import           Data.Scientific (Scientific, toBoundedRealFloat)
import           Money.Aeson()
import           GHC.TypeLits (KnownSymbol)
import           Validation (Validation (..), failure, validationToEither, eitherToValidation)
import           Data.List.NonEmpty

import Errors

asString :: Value -> Validation (NonEmpty ErrorInfo) Text
asString = \case { String s -> Success s;
                   v        -> failure $ JSONBadValue "string" v }

asDate :: Value -> Validation (NonEmpty ErrorInfo) UTCTime
asDate = \case { 
    String s -> (case (decode . L.fromStrict . encodeUtf8) s :: Maybe UTCTime of
                    Nothing -> failure $ InvalidDateValueInJSON s
                    Just d -> Success d);
    v        -> failure $ JSONBadValue "date" v }
      
asMoney :: (KnownSymbol c) => Value -> Validation (NonEmpty ErrorInfo) (Y.Dense c)
asMoney = \case { 
    String s -> (case (decode . L.fromStrict . encodeUtf8) s :: forall c1. (KnownSymbol c1) => Maybe (Y.Dense c1) of
                    Nothing -> failure $ InvalidMoneyValueInJSON s
                    Just x  -> Success x);
    v        -> failure $ JSONBadValue "money" v }

asDouble :: Value -> Validation (NonEmpty ErrorInfo) Double
asDouble v = case validationToEither $ asNumber v of
  Left  e -> eitherToValidation (Left e)
  Right s -> case toBoundedRealFloat s of 
               Left  _i -> failure $ JSONBadValue "double" v
               Right  d -> Success d

asNumber :: Value -> Validation (NonEmpty ErrorInfo) Scientific
asNumber = \case { Number n -> Success n; v -> failure $ JSONBadValue "number" v }

withObject :: forall a. Text -> Value -> (Object -> Validation (NonEmpty ErrorInfo) a) -> Validation (NonEmpty ErrorInfo) a
withObject name v f = case v of { Object o -> f o; _ -> failure $ JSONBadValue name v }

withKey :: forall a. Object -> Text -> (Value -> Validation (NonEmpty ErrorInfo) a) -> Validation (NonEmpty ErrorInfo) a
withKey o k f = maybe (failure $ JSONMissingKey k) f $ M.lookup k o