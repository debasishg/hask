{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}

module DomainUtils where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as M
import qualified Money as Y

import Data.Text
import Data.Functor
import Control.Monad.Reader
import Control.Monad.Validate
import Data.Aeson (Object, Value(..), decode, FromJSON, ToJSON)
import Data.Time
import Data.Text.Encoding (encodeUtf8)
import Data.Scientific (Scientific, toBoundedInteger, toBoundedRealFloat)
import Money.Aeson

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

pushPath :: forall m a. (MonadReader Env m) => Text -> m a -> m a
pushPath path = local (\env -> env { envPath = path : envPath env })

mkErr :: forall m. (MonadReader Env m) => ErrorInfo -> m Error
mkErr info = asks envPath <&> \path -> Error (Prelude.reverse path) info

refuteErr :: forall m c. (MonadReader Env m, MonadValidate [Error] m) => ErrorInfo -> m c
refuteErr = mkErr >=> \err -> refute [err]

disputeErr :: forall m. (MonadReader Env m, MonadValidate [Error] m) => ErrorInfo -> m ()
disputeErr = mkErr >=> \err -> dispute [err]

withObject :: forall m a. (MonadReader Env m, MonadValidate [Error] m) => Text -> Value -> (Object -> m a) -> m a
withObject name v f = case v of { Object o -> f o; _ -> refuteErr $ JSONBadValue name v }

withKey :: forall m a. (MonadReader Env m, MonadValidate [Error] m) => Object -> Text -> (Value -> m a) -> m a
withKey o k f = maybe (refuteErr $ JSONMissingKey k) (pushPath k . f) $ M.lookup k o

asString :: forall m. (MonadReader Env m, MonadValidate [Error] m) => Value -> m Text
asString = \case { String s -> pure s;
                   v        -> refuteErr $ JSONBadValue "string" v }

asDate :: forall m. (MonadReader Env m, MonadValidate [Error] m) => Value -> m UTCTime
asDate = \case { String s -> (case (decode . L.fromStrict . encodeUtf8) s :: Maybe UTCTime of
                                Nothing -> refuteErr $ InvalidDateValueInJSON s
                                Just d -> pure d);
                 v        -> refuteErr $ JSONBadValue "date" v }

asMoney :: forall m. (MonadReader Env m, MonadValidate [Error] m) => Value -> m (Y.Dense "USD")
asMoney = \case { String s -> (case (decode . L.fromStrict . encodeUtf8) s :: Maybe (Y.Dense "USD") of
                                 Nothing -> refuteErr $ InvalidMoneyValueInJSON s
                                 Just x  -> pure x);
                  v        -> refuteErr $ JSONBadValue "money" v }

asNumber :: forall m. (MonadReader Env m, MonadValidate [Error] m) => Value -> m Scientific
asNumber = \case { Number n -> pure n; v -> refuteErr $ JSONBadValue "number" v }

asInteger :: forall m. (MonadReader Env m, MonadValidate [Error] m) => Value -> m Integer
asInteger v = asNumber v >>=
  maybe (refuteErr $ JSONBadValue "integer" v) (pure . toInteger) . toBoundedInteger @Int

asDouble :: forall m. (MonadReader Env m, MonadValidate [Error] m) => Value -> m Double
asDouble v = asNumber v >>= \s ->
  case toBoundedRealFloat s of 
    Left  i -> refuteErr $ JSONBadValue "double" v
    Right d -> pure d
