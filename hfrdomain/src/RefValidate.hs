{-# OPTIONS_GHC -foptimal-applicative-do #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module RefValidate where

import Data.Time
import Data.Text (Text)
import Data.Functor
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import Control.Monad
import Control.Monad.Validate
import Control.Monad.Reader
import Data.Aeson (Object, Value(..))
import Data.Aeson.QQ (aesonQQ)

data BankAccount = BankAccount {
    accountNo          :: Text
  , accountHolderName  :: Text  
} deriving (Show)

-- data Env = Env 
--   { curTime :: !ZonedTime
--   , envPath :: [Text] }
--   deriving (Show)

data Env = Env 
  { envPath :: [Text] }
  deriving (Show, Eq)

data Error = Error { errPath :: [Text], errInfo :: ErrorInfo }
  deriving (Show, Eq)
data ErrorInfo
  = JSONBadValue Text Value
  | JSONMissingKey Text
  | InvalidAccountNumber Text 
  | InvalidAccountName Text
  | InvalidAccountOpenDate Text
  | InvalidAccountOpenCloseDateCombination Text
  deriving (Show, Eq)

makeBankAccount :: forall m. (MonadReader Env m, MonadValidate [Error] m) => Value -> m BankAccount
makeBankAccount req = withObject "request" req $ \o -> do
  accNo           <- withKey o "account_no" parseAccountNo
  accName         <- withKey o "account_name" parseAccountName
  pure BankAccount { accountNo = accNo, accountHolderName = accName } 
  where
    parseAccountNo v = do
      str <- asString v
      if T.length str /= 10
        then refuteErr $ InvalidAccountNumber str
        else pure str

    parseAccountName v = do
      str <- asString v
      if T.null str
        then refuteErr $ InvalidAccountName str
        else pure str

    pushPath :: Text -> m a -> m a
    pushPath path = local (\env -> env { envPath = path : envPath env })
    mkErr info = asks envPath <&> \path -> Error (reverse path) info
    refuteErr = mkErr >=> \err -> refute [err]
    asString = \case { String s -> pure s; v -> refuteErr $ JSONBadValue "string" v }
    disputeErr :: ErrorInfo -> m ()
    disputeErr = mkErr >=> \err -> dispute [err]

    withObject :: Text -> Value -> (Object -> m a) -> m a
    withObject name v f = case v of { Object o -> f o; _ -> refuteErr $ JSONBadValue name v }

    withKey :: Object -> Text -> (Value -> m a) -> m a
    withKey o k f = maybe (refuteErr $ JSONMissingKey k) (pushPath k . f) $ M.lookup k o

bankAccount = do
  let env = Env []
      testcase input = runReader (runValidateT (makeBankAccount input)) env
  testcase [aesonQQ| { "account_no": "1234567890", "account_name": "debasish" } |]
  testcase [aesonQQ| { "account_no": "12567890", "account_name": "debasish" } |]
  testcase [aesonQQ| { "account_no": "125678", "account_name": "" } |]
