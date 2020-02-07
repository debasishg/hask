{-# LANGUAGE TemplateHaskell #-}

module LearnLens2 where

import Control.Lens

data Ship =
  Ship { _name :: String
       , _numCrew :: Int
       } deriving (Show)

makeLenses ''Ship

data Temperature =
  Temperature { _location :: String
              , _celsius  :: Float
              } deriving (Show)

makeLenses ''Temperature

temp :: Temperature
temp = Temperature "Berlin" 7.0

-- celsius :: Lens' Temperature Float

celsiusToFahrenheit :: Float -> Float 
celsiusToFahrenheit c = (c * (9/5)) + 32

fahrenheitToCelsius :: Float -> Float 
fahrenheitToCelsius f = (f - 32) * (5/9)

fahrenheit :: Lens' Temperature Float 
fahrenheit = lens getter setter
  where
    getter = celsiusToFahrenheit . view celsius
    setter tempr f = set celsius (fahrenheitToCelsius f) tempr

main :: IO ()
main = 
  let t = view celsius temp
      u = set celsius 12.2 temp
      v = over celsius (+10) temp
      w = (celsiusToFahrenheit . view celsius) temp   -- view the temperature in Fahrenheit
      z = set celsius (fahrenheitToCelsius 56) temp   -- set to 56 Fahrenheit
      a = over celsius (fahrenheitToCelsius . (+18) . celsiusToFahrenheit) temp   -- bump by 18 Fahrenheit
      b = view fahrenheit temp
      c = set fahrenheit 56 temp
      d = over fahrenheit (+18) temp
  in print (t, u, v, w, z, a, b, c, d)