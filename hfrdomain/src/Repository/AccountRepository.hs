{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Repository.AccountRepository where 

import qualified Money as Y
import qualified Data.Text as T
import           Data.Time
import           Data.Maybe (listToMaybe)
import           Polysemy          
import           Polysemy.Input
import           Database.SQLite.Simple 
import           Database.SQLite.Simple.FromField 
import           Database.SQLite.Simple.ToField 
import           Database.SQLite.Simple.Internal 
import           Database.SQLite.Simple.Ok 
import qualified Database.SQLite.Simple as SQL


import Model.Account
import Model.AccountType
import Model.Schema

-- | Repository abstraction that's independent of the underlying database 
-- representation
data AccountRepository m a where
    QueryAccount         :: T.Text -> AccountRepository m (Maybe Account)
    Store                :: Account -> AccountRepository m ()
    -- QueryByOpenDate   :: UTCTime -> AccountRepository m [Account]
    -- AllAccounts       :: AccountRepository m [Account]
    -- Upsert            :: Account -> AccountRepository m Account

makeSem ''AccountRepository

instance ToField AccountType where
  toField = SQLText . T.pack . show
  
instance FromField AccountType where
  fromField (Field (SQLText "Ch") _) = Ok Ch
  fromField (Field (SQLText "Sv") _) = Ok Sv
  fromField f = returnError ConversionFailed f "need 'Ch' or 'Sv'"

instance ToField MoneyUSD where
  toField = SQLText . T.pack . show

instance FromField MoneyUSD where
  fromField f@(Field (SQLText s) _) = 
    case Y.denseFromDecimal Y.defaultDecimalConf s of  
      Just x -> Ok x
      Nothing -> returnError ConversionFailed f "Invalid Money"
  fromField f = returnError ConversionFailed f "Invalid Money"
  
instance FromRow Account where
  fromRow = Account <$> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow Account where
  toRow (Account ano atype aname aodate acdate abal arate) = toRow (ano, atype, aname, aodate, acdate, abal, arate)

-- CREATE TABLE "account"
-- ("account_no" VARCHAR NOT NULL,
--  "account_type" varchar NOT NULL,
--  "account_holder_name" VARCHAR NOT NULL,
--  "account_open_date" TIMESTAMP NOT NULL DEFAULT CURRENT_TIME,
--  "account_close_date" TIMESTAMP NULL DEFAULT NULL,
--  "current_balance" varchar NOT NULL,
--  "rate_of_interest" REAL NOT NULL, PRIMARY KEY ("account_no"))

runAccountRepository :: Member (Embed IO) r => Sem (AccountRepository ': r) a -> Sem (Input SQL.Connection ': r) a
runAccountRepository = reinterpret $ \case
  QueryAccount ano -> do
    conn <- input
    account <- embed $ SQL.queryNamed conn
              "SELECT * FROM account WHERE account_no = :accountno"
              [":accountno" := ano]
    return $ listToMaybe account

  Store a -> do
    conn <- input
    embed $ execute conn "INSERT INTO account (account_no, account_type, account_holder_name, account_open_date, account_close_date, current_balance, rate_of_interest) VALUES (?, ?, ?, ?, ?, ?, ?)" (a)