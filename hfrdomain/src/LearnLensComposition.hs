{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module LearnLensComposition where

import Control.Lens
import Data.Text

data Person =
  Person { _name :: Text
         , _address :: Address
         } deriving (Show)

data Address =
  Address { _streetAddress :: StreetAddress
          , _city :: Text 
          , _country :: Text 
          } deriving (Show)

data StreetAddress =
  StreetAddress { _streetNumber :: Text
                , _streetName   :: Text
                } deriving (Show)

makeLenses ''Person 
makeLenses ''Address 
makeLenses ''StreetAddress

sherlock :: Person 
sherlock =
  Person { _name = "S. Holmes" 
         , _address = Address { _streetAddress = StreetAddress { _streetNumber = "221B"
                                                               , _streetName = "Baker Street"
                                                               }
                              , _city = "London" 
                              , _country = "England" 
                              }
         }

-- :t streetNumber
-- > streetNumber
--   :: Functor f =>
--      (String -> f String) -> StreetAddress -> f StreetAddress
-- ~ Lens' StreetAddress String

-- :t streetAddress
-- > streetAddress
--   :: Functor f =>
--      (StreetAddress -> f StreetAddress) -> Address -> f Address
-- ~ Lens' Address StreetAddress

-- :t streetAddress . streetNumber
-- streetAddress . streetNumber
--   :: Functor f => (String -> f String) -> Address -> f Address
-- ~ Lens' Address String

cityOfAddress :: Lens' Person Text
cityOfAddress = address . city

main :: IO ()
main = 
  let t = view address sherlock

      -- update city
      u = over address (\a -> Address { _streetAddress = _streetAddress a, _country = _country a, _city = toUpper (_city a) }) sherlock

      -- update city with lens composition
      v = over cityOfAddress toUpper sherlock

  in print (t, u, v)