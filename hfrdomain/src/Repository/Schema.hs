{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DataKinds                  #-}

module Repository.Schema where

import qualified Money as Y

import Data.Time
import Data.Text (Text)
import Money.Aeson()
import Database.Persist.TH 
import Database.Persist.Sqlite 

import Repository.AccountType
import PersistentMoney()

type MoneyUSD = (Y.Dense "USD")

share 
    [ mkPersist sqlSettings { mpsGenerateLenses = True, mpsPrefixFields = False }
    , mkMigrate "migrateAll"
    ] [persistLowerCase|
Account json  
    accountNo           Text
    accountType         AccountType sqltype=varchar  
    accountHolderName   Text  
    accountOpenDate     UTCTime default=CURRENT_TIME
    accountCloseDate    UTCTime Maybe default=NULL
    currentBalance      MoneyUSD
    rateOfInterest      Double 
    deriving Show 
|]

makeFullAccount :: Text
    -> AccountType
    -> Text
    -> UTCTime
    -> Maybe UTCTime
    -> Y.Dense "USD"
    -> Double
    -> Account
makeFullAccount accNo accType accName accOpenDate accCloseDate currBalance rateOfInt =
    Account {
          _accountNo = accNo
        , _accountType = accType
        , _accountHolderName = accName
        , _accountOpenDate = accOpenDate
        , _accountCloseDate = accCloseDate
        , _currentBalance = currBalance
        , _rateOfInterest = rateOfInt 
    }