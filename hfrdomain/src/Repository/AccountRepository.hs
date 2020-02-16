{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleInstances          #-}
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

runAccountRepository :: Member (Embed IO) r => Sem (AccountRepository ': r) a -> Sem (Input SQL.Connection ': r) a
runAccountRepository = reinterpret $ \case
  QueryAccount ano -> do
    conn <- input
    account <- embed $ SQL.queryNamed conn
              "SELECT * FROM account WHERE accountno = :accountno"
              [":accountno" := ano]
    return $ listToMaybe account

  Store a -> do
    conn <- input
    embed $ execute conn "INSERT INTO account (accountno, accounttype, accountholdername, accountopendate, accountclosedate, currentbalance, rateofinterest) VALUES (?, ?, ?, ?, ?, ?, ?)" (a)