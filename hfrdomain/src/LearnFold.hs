{-# LANGUAGE TemplateHaskell #-}

module LearnFold where

import Control.Lens
import qualified Data.Set as S


data Role
  = Gunner 
  | PowderMonkey 
  | Navigator 
  | Captain 
  | FirstMate 
  deriving (Show, Eq, Ord)

data CrewMember = 
  CrewMember { _name :: String 
             , _role :: Role 
             , _talents :: [String] 
             }
  deriving (Show, Eq, Ord) 
  
makeLenses ''CrewMember

roster :: S.Set CrewMember 
roster = S.fromList
--             Name           Role   Talents 
  [ CrewMember "Grumpy Roger" Gunner ["Juggling", "Arbitrage"] 
  , CrewMember "Long-John Bronze" PowderMonkey ["Origami"] 
  , CrewMember "Salty Steve" PowderMonkey ["Charcuterie"] 
  , CrewMember "One-eyed Jack" Navigator [] 
  ]

-- folded :: Foldable f => Fold (f a) a
crewMembers :: Fold (S.Set CrewMember) CrewMember 
crewMembers = folded

-- toListOf :: Fold s a -> s -> [a]
-- (^..) :: s -> Fold s a -> [a]

-- toListOf folded roster
-- > [CrewMember {_name = "Grumpy Roger", _role = Gunner, _talents = ["Juggling","Arbitrage"]},CrewMember {_name = "Long-John Bronze", _role = PowderMonkey, _talents = ["Origami"]},CrewMember {_name = "One-eyed Jack", _role = Navigator, _talents = []},CrewMember {_name = "Salty Steve", _role = PowderMonkey, _talents = ["Charcuterie"]}]

crewRole :: Fold CrewMember Role
crewRole = role

newtype Name = Name
    { getName' :: String 
    } deriving Show

data ShipCrew = ShipCrew 
  { _shipName :: Name 
  , _captain :: Name 
  , _firstMate :: Name 
  , _conscripts :: [Name] 
  } deriving (Show) 
  
makeLenses ''ShipCrew

-- folding :: Foldable f => (s -> f a) -> Fold s a

collectCrewMembers :: ShipCrew -> [Name] 
collectCrewMembers crew = [_captain crew, _firstMate crew] ++ _conscripts crew

crewMembers' :: Fold ShipCrew Name 
crewMembers' = folding collectCrewMembers

myCrew :: ShipCrew 
myCrew =
  ShipCrew 
    { _shipName = Name "Purple Pearl" 
    , _captain = Name "Grumpy Roger" 
    , _firstMate = Name "Long-John Bronze" 
    , _conscripts = [Name "One-eyed Jack", Name "Filthy Frank"] 
    }