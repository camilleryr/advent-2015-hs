module Puzzle.Day6 where

import Data.List (foldl')
import Data.List.Utils
import Data.Map (Map, empty, fold, insertWith)
import Data.Set (Set, delete, difference, empty, fromList, insert, member, size, union)
import Input.Day6

type Point = (,) Integer Integer

data Action = TurnOn | TurnOff | Toggle deriving (Show, Eq)

data Command = Command Action Point Point deriving (Show, Eq)

part1 :: String -> Maybe Int
part1 = fmap (size . foldCommands) . parse

part2 :: String -> Maybe Int
part2 = fmap ((foldl' (+) 0) . foldCommands') . parse

foldCommands' :: [Command] -> Map Point Int
foldCommands' = foldl' performCommand Data.Map.empty
  where
    performCommand acc (Command TurnOn a b) = updateAcc (\map point -> insertWith (+) point 1 map) acc a b
    performCommand acc (Command TurnOff a b) = updateAcc (\map point -> insertWith (\_ o -> if o > 0 then o - 1 else 0) point 0 map) acc a b
    performCommand acc (Command Toggle a b) = updateAcc (\map point -> insertWith (+) point 2 map) acc a b
    updateAcc f acc x y = foldl' f acc (build x y)

build :: Point -> Point -> [Point]
build (x', y') (x'', y'') = [(x, y) | x <- [x' .. x''], y <- [y' .. y'']]

foldCommands :: [Command] -> Set Point
foldCommands = foldl' performCommand Data.Set.empty
  where
    performCommand acc (Command TurnOn a b) = updateAcc (\set -> union set . fromList) acc a b
    performCommand acc (Command TurnOff a b) = updateAcc (\set -> difference set . fromList) acc a b
    performCommand acc (Command Toggle a b) = updateAcc (\set list -> foldl' (\x y -> if (member y x) then (delete y x) else (insert y x)) set list) acc a b
    updateAcc f acc x y = f acc (build x y)

parse :: String -> Maybe [Command]
parse = sequence . map (doParse . reverse . split " " . replace "," " ") . split "\n"
  where
    doParse [t, u, "through", w, x, "on", "turn"] = Just (Command TurnOn (read x :: Integer, read w :: Integer) (read u :: Integer, read t :: Integer))
    doParse [t, u, "through", w, x, "off", "turn"] = Just (Command TurnOff (read x :: Integer, read w :: Integer) (read u :: Integer, read t :: Integer))
    doParse [t, u, "through", w, x, "toggle"] = Just (Command Toggle (read x :: Integer, read w :: Integer) (read u :: Integer, read t :: Integer))
    doParse _ = Nothing
