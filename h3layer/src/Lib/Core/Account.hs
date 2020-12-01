{-# LANGUAGE DeriveAnyClass #-}

module Lib.Core.Account
       ( Account (..)
       , mkAccount
       ) where

import qualified Data.Text as T

import Data.List.NonEmpty (head)

import Data.Time (UTCTime)
import Lib.Core.DomainError (DomainError (InvalidAccountName, InvalidAccountNo, InvalidAccountOpenCloseDateCombination, InvalidAccountOpenDate))
import Lib.Core.Id (Id (Id))
import Lib.Core.User (User)
import Validation (Validation (..), failure, failureIf)

-- | Data type representing row in the @accounts@ table.
data Account = Account
    { accountNo   :: !Text
    , accountName :: !Text
    , openDate    :: !UTCTime
    , closeDate   :: !(Maybe UTCTime)
    , userId      :: !(Id User)
    } deriving stock (Generic, Show, Eq)
      deriving anyclass (FromRow)
      deriving (FromJSON, ToJSON) via Account

validateAccountNumber :: Text -> Validation (NonEmpty DomainError) Text
validateAccountNumber ano = ano <$ failureIf (T.length ano /= 10) (InvalidAccountNo ano)

validateAccountName :: Text -> Validation (NonEmpty DomainError) Text
validateAccountName nm = nm <$ failureIf (T.null nm) (InvalidAccountName nm)

validateAccountOpenDate :: UTCTime -> UTCTime -> Validation (NonEmpty DomainError) UTCTime
validateAccountOpenDate current d = d <$ failureIf (current < d) (InvalidAccountOpenDate (T.pack $ "Account open date " ++ show d ++ " " ++ show current ++ " cannot be in future"))

validateAccountOpenCloseDate :: Account -> Validation (NonEmpty DomainError) Account
validateAccountOpenCloseDate acc = case closeDate acc of
  Nothing -> Success acc
  Just dt -> acc <$ failureIf (openDate acc > dt) (InvalidAccountOpenCloseDateCombination (T.pack $ "Account close date " ++ show dt ++ " cannot precede open date " ++ show (openDate acc)))

_mkAccount :: Text -> Text -> UTCTime -> UTCTime -> Maybe UTCTime -> Text -> Validation (NonEmpty DomainError) Account
_mkAccount no name currDate odate cdate uid = Account
    <$> validateAccountNumber no
    <*> validateAccountName name
    <*> validateAccountOpenDate currDate odate
    <*> pure cdate
    <*> pure (Id uid)

validate :: Account -> Validation (NonEmpty DomainError) Account
validate = validateAccountOpenCloseDate

mkAccount :: Text -> Text -> UTCTime -> UTCTime -> Maybe UTCTime -> Text -> Validation (NonEmpty DomainError) Account
mkAccount no name currdate odate cdate uid =
    case _mkAccount no name currdate odate cdate uid of
        Success acc -> validate acc
        Failure e   -> failure $ Data.List.NonEmpty.head e
