{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Refdata where

import Data.Time
import Data.Validation
import Control.Monad.Reader

newtype Money = Money
  { unMoney :: Double
  } deriving (Show, Eq, Num)

type NetAmount = Money

data Instrument
  = Equity 
  | FixedIncome
  deriving (Show)

data Equity = EQ {
    isinEq             :: String
  , eqName             :: String
  , eqIssueDate        :: ZonedTime
  , issuePrice         :: Money
}

data FixedIncome = FI {
    isinFI             :: String
  , fiName             :: String
  , fiIssueDate        :: ZonedTime
  , couponRate         :: Money
  , faceValue          :: Money
}

data Account = Account {
    accountNo          :: String
  , accountHolderName  :: String  
  , openingDate        :: ZonedTime
  , closingDate        :: Maybe ZonedTime
} deriving (Show)

data Env = Env { curTime :: !ZonedTime }

validateAccountNo :: String -> Reader Env (Validation [String] String)
validateAccountNo accNo = 
  if length accNo /= 10
    then return $ Failure ["Account Number has to be of 10 characters"]
    else return $ Success accNo

validateAccountName :: String -> Reader Env (Validation [String] String)
validateAccountName name =
  if null name 
    then return $ Failure ["Name cannot be empty"]
    else return $ Success name

validateOpenDate :: ZonedTime -> Reader Env (Validation [String] ZonedTime)
validateOpenDate openDate = do
  now <- asks curTime
  if zonedTimeToUTC openDate > zonedTimeToUTC now
  then return $ Failure ["Account open date cannot be in the future"]
  else return $ Success openDate

validateOpenCloseDate :: ZonedTime -> Maybe ZonedTime -> Reader Env (Validation [String] (Maybe ZonedTime))
validateOpenCloseDate _openDate Nothing = return $ Success Nothing
validateOpenCloseDate openDate (Just closeDate) = 
  if zonedTimeToUTC openDate <= zonedTimeToUTC closeDate
    then return $ Success (Just closeDate)
    else return $ Failure ["Account open date has to precede close date"]

makeAccount :: String -> String -> ZonedTime -> Maybe ZonedTime -> IO (Validation [String] Account)
makeAccount no name openDate maybeCloseDate = do
  env <- Env <$> getZonedTime
  flip runReader env $ do 
    validAccountNo       <- validateAccountNo no
    validAccountName     <- validateAccountName name
    validOpeningDate     <- validateOpenDate openDate
    validCloseDate       <- validateOpenCloseDate openDate maybeCloseDate
    return $ 
      return $ 
        Account <$> 
              validAccountNo 
          <*> validAccountName 
          <*> validOpeningDate 
          <*> validCloseDate 

account :: IO (Validation [String] Account, Validation [String] Account) 
account = do
  od <- getZonedTime 
  invalid <- makeAccount "123" "" od (Just od)
  valid <- makeAccount "0123456789" "ram" od Nothing
  return (invalid, valid)

data Market = NewYork
            | Singapore
            | HongKong
            | Tokyo
            | Other
