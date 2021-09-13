{-# LANGUAGE QuasiQuotes #-}

module Puzzle.Day9 where

import Data.List (minimum, nub, permutations, sort)
import Data.List.Split (splitOn)
import Data.Map (Map, empty, insert, (!))
import Input.Day9
import Text.RawString.QQ (r)

test1 =
  [r|London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141|]

type City = String

type Distances = Map [City] Int

part1 :: String -> Maybe Int
part1 = fmap (minimum . tsp) . parse

part2 :: String -> Maybe Int
part2 = fmap (maximum . tsp) . parse

tsp :: ([City], Distances) -> [Int]
tsp (cities, distances) = map (getDistance 0) . permutations $ cities
  where
    getDistance acc (x : y : rest) = getDistance (acc + (distances ! sort [x, y])) (y : rest)
    getDistance acc _ = acc

parse :: String -> Maybe ([City], Distances)
parse = fmap (foldl (\(xs, d) (x, y) -> (nub . sort $ x ++ xs, insert x y d)) ([], empty)) . mapM (doParse . splitOn " ") . splitOn "\n"
  where
    doParse [x, "to", y, "=", z] = Just (sort [x, y], read z :: Int)
    doParse _ = Nothing
