{-# LANGUAGE QuasiQuotes #-}

module Puzzle.Day14 where

import Data.Function (on)
import Data.List (maximumBy)
import Data.List.Split (splitOn)
import Data.Map (Map, empty, foldrWithKey, insertWith)
import Text.RawString.QQ (r)

test :: String
test =
  [r|Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.|]

input :: String
input =
  [r|Vixen can fly 19 km/s for 7 seconds, but then must rest for 124 seconds.
Rudolph can fly 3 km/s for 15 seconds, but then must rest for 28 seconds.
Donner can fly 19 km/s for 9 seconds, but then must rest for 164 seconds.
Blitzen can fly 19 km/s for 9 seconds, but then must rest for 158 seconds.
Comet can fly 13 km/s for 7 seconds, but then must rest for 82 seconds.
Cupid can fly 25 km/s for 6 seconds, but then must rest for 145 seconds.
Dasher can fly 14 km/s for 3 seconds, but then must rest for 38 seconds.
Dancer can fly 3 km/s for 16 seconds, but then must rest for 37 seconds.
Prancer can fly 25 km/s for 6 seconds, but then must rest for 143 seconds.|]

data Score = Score Int Int deriving (Show)

data Flight = Flight Int Int deriving (Show)

newtype Rest = Rest Int deriving (Show)

data Reindeer = Reindeer String Flight Rest deriving (Show)

part1 :: Int -> String -> Maybe Int
part1 x = fmap (maximum . fmap (move x)) . parse

part2 :: Int -> String -> Maybe Int
part2 x = fmap (move' x) . parse

move' :: Int -> [Reindeer] -> Int
move' totalSteps reindeer = foldl greatest 0 . fst . foldl run (empty, empty) $ [1 .. totalSteps]
  where
    greatest y z = if y > z then y else z
    run :: (Map String Int, Map String Int) -> Int -> (Map String Int, Map String Int)
    run (points, distances) step = (updatedPoints, updatedDistances)
      where
        updatedDistances = foldl (updateDistances step) distances reindeer
        updatedPoints = foldl (\a s -> insertWith (+) s 1 a) points (getWinners updatedDistances)

getWinners :: Map String Int -> [String]
getWinners =
  fst
    . foldrWithKey
      ( \name distance acc@(names, greatest) -> case compare distance greatest of
          LT -> acc
          EQ -> (name : names, greatest)
          GT -> ([name], distance)
      )
      ([], 0)

updateDistances :: Int -> Map String Int -> Reindeer -> Map String Int
updateDistances x distances reindeer = insertWith (+) (getName reindeer) (getTraveled x reindeer) distances

getName :: Reindeer -> String
getName (Reindeer name _ _) = name

getTraveled :: Int -> Reindeer -> Int
getTraveled x (Reindeer _ (Flight speed flightDurration) (Rest restDurration)) = if mod (x - 1) (flightDurration + restDurration) < flightDurration then speed else 0

move :: Int -> Reindeer -> Int
move total (Reindeer _ (Flight speed flightDurration) (Rest restDurration)) = go 0 0
  where
    go x y
      | x == total = y
      | mod x (flightDurration + restDurration) < flightDurration = go (x + 1) (y + speed)
      | otherwise = go (x + 1) y

parse :: String -> Maybe [Reindeer]
parse = mapM (doParse . words) . splitOn "\n"
  where
    doParse [name, _, _, speed, _, _, time, _, _, _, _, _, _, rest, _] = Just (Reindeer name (Flight (read speed :: Int) (read time :: Int)) (Rest (read rest :: Int)))
    doParse _ = Nothing
