module Lib.Core.DomainError where

data DomainError
    = InvalidAccountNo !Text
    | InvalidAccountName !Text
    | InvalidAccountOpenDate !Text
    | InvalidAccountCloseDate !Text
    | InvalidAccountOpenCloseDateCombination !Text
    deriving (Show, Eq)
