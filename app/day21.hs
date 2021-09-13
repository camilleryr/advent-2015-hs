{-# LANGUAGE QuasiQuotes #-}

module Puzzle.Day21 where

import Data.Function (on)
import Data.List (find, sortBy)
import Data.Maybe (isNothing)
import Text.RawString.QQ (r)

newtype Armor = Armor Int deriving (Show, Eq)

newtype HitPoints = HitPoints Int deriving (Show, Eq)

newtype Damage = Damage Int deriving (Show, Eq)

newtype Cost = Cost Int deriving (Show, Eq)

data Weapons = Dagger | Shortsword | Warhammer | Longsword | Greataxe deriving (Show, Eq)

data Rings = Damage1 | Damage2 | Damage3 | Defense1 | Defense2 | Defense3 deriving (Show, Eq)

data Armors = Leather | Chainmail | Splintmail | Bandedmail | Platemail deriving (Show, Eq)

data Item category = Item category Cost Damage Armor deriving (Show, Eq)

type Inventory = (Maybe (Item Weapons), Maybe (Item Armors), (Maybe (Item Rings), Maybe (Item Rings)))

data Player = Player String HitPoints Damage Armor deriving (Show)

computer :: Player
computer = Player "Computer" (HitPoints 109) (Damage 8) (Armor 2)

player :: Player
player = Player "Player" (HitPoints 100) (Damage 0) (Armor 0)

testComputer :: Player
testComputer = Player "Test Computer" (HitPoints 12) (Damage 7) (Armor 2)

testPlayer :: Player
testPlayer = Player "Test Player" (HitPoints 8) (Damage 5) (Armor 5)

weapons :: [Item Weapons]
weapons =
  [ Item Dagger (Cost 8) (Damage 4) (Armor 0),
    Item Shortsword (Cost 10) (Damage 5) (Armor 0),
    Item Warhammer (Cost 25) (Damage 6) (Armor 0),
    Item Longsword (Cost 40) (Damage 7) (Armor 0),
    Item Greataxe (Cost 74) (Damage 8) (Armor 0)
  ]

armors :: [Item Armors]
armors =
  [ Item Leather (Cost 13) (Damage 0) (Armor 1),
    Item Chainmail (Cost 31) (Damage 0) (Armor 2),
    Item Splintmail (Cost 53) (Damage 0) (Armor 3),
    Item Bandedmail (Cost 75) (Damage 0) (Armor 4),
    Item Platemail (Cost 102) (Damage 0) (Armor 5)
  ]

rings :: [Item Rings]
rings =
  [ Item Damage1 (Cost 25) (Damage 1) (Armor 0),
    Item Damage2 (Cost 50) (Damage 2) (Armor 0),
    Item Damage3 (Cost 100) (Damage 3) (Armor 0),
    Item Defense1 (Cost 20) (Damage 0) (Armor 1),
    Item Defense2 (Cost 40) (Damage 0) (Armor 2),
    Item Defense3 (Cost 80) (Damage 0) (Armor 3)
  ]

part1 :: Maybe Int
part1 = fmap price . find (\i' -> play (updatePlayer player i') computer) $ inventories

part2 :: Maybe Int
part2 = fmap price . find (\i' -> not (play (updatePlayer player i') computer)) . reverse $ inventories

inventories :: [Inventory]
inventories = sortBy (compare `on` price) i
  where
    i = [(w, x, (y, z)) | w <- map Just weapons, x <- Nothing : map Just armors, y <- Nothing : map Just rings, z <- Nothing : map Just rings, (isNothing y && isNothing z) || y /= z]

updatePlayer :: Player -> Inventory -> Player
updatePlayer (Player n hp (Damage d) (Armor a)) x = Player n hp (Damage (d + damage x)) (Armor (a + armor x))

damage :: Inventory -> Int
damage (w, x, (y, z)) = getDamage w + getDamage x + getDamage y + getDamage z
  where
    getDamage (Just (Item _ _ (Damage x') _)) = x'
    getDamage _ = 0

armor :: Inventory -> Int
armor (w, x, (y, z)) = getArmor w + getArmor x + getArmor y + getArmor z
  where
    getArmor (Just (Item _ _ _ (Armor x'))) = x'
    getArmor _ = 0

price :: Inventory -> Int
price (w, x, (y, z)) = getPrice w + getPrice x + getPrice y + getPrice z
  where
    getPrice (Just (Item _ (Cost x') _ _)) = x'
    getPrice _ = 0

play :: Player -> Player -> Bool
play p@(Player _ hp d a) p'@(Player _ hp' d' a') =
  turns d' a hp >= turns d a' hp'
  where
    turns (Damage y) (Armor z) (HitPoints x) = ceiling (realToFrac x / realToFrac (max (y - z) 1))
