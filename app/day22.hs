{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Puzzle.Day22 where

import Data.Function (on)
import Data.List (foldl', minimumBy, partition)
import Data.Maybe (isJust)

data SpellType = MagicMissile | Drain | Shield | Poison | Recharge deriving (Show, Eq)

data Spell = Spell
  { spell :: SpellType,
    cost :: Int
  }
  deriving (Show)

data Effect = Effect
  { spell' :: SpellType,
    duration :: Int
  }
  deriving (Show)

data PlayerType = Human | Computer deriving (Show, Eq)

data Player = Player
  { name :: PlayerType,
    currentMana :: Int,
    manaSpent :: Int,
    armor :: Int,
    damage :: Int,
    hitPoints :: Int
  }
  deriving (Show)

data Game = Game
  { player1 :: Player,
    player2 :: Player,
    effects :: [Effect],
    turn :: Int,
    winner :: Maybe Player
  }
  deriving (Show)

game :: Player -> Player -> Game
game p1 p2 = Game {player1 = p1, player2 = p2, effects = [], turn = 0, winner = Nothing}

human :: Player
human = Player {name = Human, currentMana = 500, hitPoints = 50, manaSpent = 0, armor = 0, damage = 0}

computer :: Player
computer = Player {name = Computer, damage = 9, hitPoints = 51, currentMana = 0, manaSpent = 0, armor = 0}

part1 :: Player -> Player -> Game
part1 p1 p2 = findCheapestWin id Nothing [g]
  where
    g = game p1 p2

part2 :: Player -> Player -> Game
part2 p1 p2 = findCheapestWin looseOneHp Nothing [g]
  where
    g = game p1 p2
    looseOneHp g'@Game {player1 = p@Player {hitPoints = hp}} = applyWinner (g' {player1 = p {hitPoints = hp - 1}})

findCheapestWin :: (Game -> Game) -> Maybe Game -> [Game] -> Game
findCheapestWin _ Nothing [] = undefined
findCheapestWin _ (Just winner) [] = winner
findCheapestWin f currentWinner pending = findCheapestWin f newWinner stillPending
  where
    newWinner = getNewWinner currentWinner won
    (won, stillPending) = partition (\Game {winner = w} -> isJust w) next
    next = concatMap (expand f) . filterToExpensive currentWinner $ pending

getNewWinner :: Maybe Game -> [Game] -> Maybe Game
getNewWinner x [] = x
getNewWinner Nothing y = Just (cheapest y)
getNewWinner (Just x) y = Just (cheapest (x : y))

cheapest :: [Game] -> Game
cheapest = minimumBy (compare `on` (manaSpent . player1))

filterToExpensive :: Maybe Game -> [Game] -> [Game]
filterToExpensive Nothing = id
filterToExpensive (Just Game {player1 = Player {manaSpent = ms}}) = filter (\Game {player1 = Player {manaSpent = ms'}} -> ms' < ms)

expand :: (Game -> Game) -> Game -> [Game]
expand f g@Game {player1 = Player {currentMana = cm}, effects = es} = filter notComputerWon . map (\sp -> applyTurn f sp g) . filter (\Spell {spell = s, cost = c} -> c <= cm && notElem s invalidSpells) $ allSpells
  where
    invalidSpells = foldl' (\acc Effect {spell' = s', duration = d} -> if d > 1 then s' : acc else acc) [] es
    notComputerWon Game {winner = Nothing} = True
    notComputerWon Game {winner = (Just Player {name = Human})} = True
    notComputerWon Game {winner = (Just Player {name = Computer})} = False

human' :: Player
human' = Player {name = Human, currentMana = 250, hitPoints = 10, manaSpent = 0, armor = 0, damage = 0}

computer' :: Player
computer' = Player {name = Computer, damage = 8, hitPoints = 13, currentMana = 0, manaSpent = 0, armor = 0}

computer'' :: Player
computer'' = Player {name = Computer, damage = 8, hitPoints = 14, currentMana = 0, manaSpent = 0, armor = 0}

recharge :: Spell
recharge = Spell {spell = Recharge, cost = 229}

poison :: Spell
poison = Spell {spell = Poison, cost = 173}

shield :: Spell
shield = Spell {spell = Shield, cost = 113}

drain :: Spell
drain = Spell {spell = Drain, cost = 73}

magicMissile :: Spell
magicMissile = Spell {spell = MagicMissile, cost = 53}

allSpells :: [Spell]
allSpells = [magicMissile, drain, shield, poison, recharge]

applyTurn :: (Game -> Game) -> Spell -> Game -> Game
applyTurn _ _ g@Game {winner = (Just _)} = g
applyTurn f s g = applyWinner g''
  where
    g' = applySpell s . applyEffects . f $ g
    g'' = computerAttack . applyEffects $ g'

applyWinner :: Game -> Game
applyWinner g@Game {winner = (Just _)} = g
applyWinner g@Game {player1 = p1, player2 = p2}
  | hitPoints p1 <= 0 = g {winner = Just p2}
  | hitPoints p2 <= 0 = g {winner = Just p1}
  | otherwise = g

applySpell :: Spell -> Game -> Game
applySpell _ g@Game {winner = (Just _)} = g
applySpell s g' = applySpell' s . pay s $ g'
  where
    pay Spell {cost = x} g@Game {player1 = p1@Player {currentMana = y, manaSpent = z}} = g {player1 = p1 {currentMana = y - x, manaSpent = z + x}}
    applySpell' Spell {spell = MagicMissile} g@Game {player2 = p2} = g {player2 = p2 {hitPoints = hitPoints p2 - 4}}
    applySpell' Spell {spell = Drain} g@Game {player1 = p1, player2 = p2} = g {player1 = p1 {hitPoints = hitPoints p1 + 2}, player2 = p2 {hitPoints = hitPoints p2 - 2}}
    applySpell' Spell {spell = Shield} g@Game {player1 = p1, effects = e} = g {player1 = p1 {armor = armor p1 + 7}, effects = Effect {spell' = Shield, duration = 6} : e}
    applySpell' Spell {spell = Poison} g@Game {effects = e} = g {effects = Effect {spell' = Poison, duration = 6} : e}
    applySpell' Spell {spell = Recharge} g@Game {effects = e} = g {effects = Effect {spell' = Recharge, duration = 5} : e}

computerAttack :: Game -> Game
computerAttack g@Game {winner = (Just _)} = g
computerAttack g@Game {player1 = p1, player2 = p2} = g {player1 = p1 {hitPoints = hp'}}
  where
    hp' = if hitPoints p2 > 0 then hitPoints p1 - max (damage p2 - armor p1) 1 else hitPoints p1

applyEffects :: Game -> Game
applyEffects g@Game {winner = (Just _)} = g
applyEffects g@Game {effects = effects} = foldl' doApplyEffect (g {effects = []}) effects
  where
    doApplyEffect g'@Game {player1 = player1} Effect {spell' = Shield, duration = 0} = g' {player1 = player1 {armor = armor player1 - 7}}
    doApplyEffect g' Effect {duration = 0} = g'
    doApplyEffect g'@Game {player2 = p2, effects = es} e@Effect {spell' = Poison, duration = d} = g' {player2 = p2 {hitPoints = hitPoints p2 - 3}, effects = e {duration = d - 1} : es}
    doApplyEffect g'@Game {player1 = p1, effects = es} e@Effect {spell' = Recharge, duration = d} = g' {player1 = p1 {currentMana = currentMana p1 + 101}, effects = e {duration = d - 1} : es}
    doApplyEffect g'@Game {effects = es} e@Effect {duration = d} = g' {effects = e {duration = d -1} : es}
