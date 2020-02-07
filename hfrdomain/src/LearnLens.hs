module LearnLens where

import Control.Lens

data Ship =
  Ship { _name :: String
       , _numCrew :: Int
       } deriving (Show)

-- lens :: (s -> a) -> (s -> a -> s) -> Lens' s a

-- a lens for the _numCrew field
getNumCrew :: Ship -> Int 
getNumCrew = _numCrew -- getter part of the lens

setNumCrew :: Ship -> Int -> Ship
setNumCrew s a = s { _numCrew = a } -- setter part of the lens

numCrew :: Lens' Ship Int
numCrew = lens getNumCrew setNumCrew

-- a lens for the _name field
getName :: Ship -> String
getName = _name

setName :: Ship -> String -> Ship
setName s n = s { _name = n }

name :: Lens' Ship String
name = lens getName setName

purplePearl :: Ship 
purplePearl = Ship { _name = "Purple Pearl" , _numCrew = 38 }

main :: IO ()
main = 
  let a = view (_2 . _1) (42, ("hello", False )) -- view :: Lens' s a -> (s -> a)
      b = set _1 'x' ('a', 'b')                  -- set :: Lens' s a -> a -> s -> s
      c = over _1 (*100) (1, 2)                  -- over :: Lens' s a -> (a -> a) -> s -> s
      d = view numCrew purplePearl               -- 38 
      e = set numCrew 41 purplePearl             -- Ship { _name = "Purple Pearl" , _numCrew = 41 }
      f = over numCrew (+3) purplePearl          -- Ship { _name = "Purple Pearl" , _numCrew = 41 }

  in  print (f)                         -- ("hello", ('x', 'b'), (100, 2), 38)