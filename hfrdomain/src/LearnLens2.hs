{-# LANGUAGE TemplateHaskell #-}

module LearnLens2 where

import Control.Lens

data Ship =
  Ship { _name :: String
       , _numCrew :: Int
       } deriving (Show)

makeLenses ''Ship