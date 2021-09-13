{-# LANGUAGE QuasiQuotes #-}

module Puzzle.Day16 where

import Data.Map (Map, empty, filter, findMin, findWithDefault, fromList, insert, toList)
import Text.RawString.QQ (r)

part1 :: String -> Maybe String
part1 = fmap (fst . findMin . findSue) . parse

findSue :: Map String (Map String Int) -> Map String (Map String Int)
findSue = Data.Map.filter doesMatch
  where
    doesMatch suesProperties = all (\(k, v) -> findWithDefault v k suesProperties == v) (toList properties)

part2 :: String -> Maybe String
part2 = fmap (fst . findMin . findSue') . parse

findSue' :: Map String (Map String Int) -> Map String (Map String Int)
findSue' = Data.Map.filter doesMatch
  where
    doesMatch suesProperties = all (matcher suesProperties) (toList properties)
    matcher x (k@"cats", v) = findWithDefault (v + 1) k x > v
    matcher x (k@"trees", v) = findWithDefault (v + 1) k x > v
    matcher x (k@"pomeranians", v) = findWithDefault (v - 1) k x < v
    matcher x (k@"goldfish", v) = findWithDefault (v - 1) k x < v
    matcher x (k, v) = findWithDefault v k x == v

properties :: Map String Int
properties =
  buildProperties . concatMap words . lines . Prelude.filter (not . flip elem ":") $
    [r|children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1|]

parse :: String -> Maybe (Map String (Map String Int))
parse = fmap fromList . mapM (doParse . words) . lines . Prelude.filter (not . flip elem ":,")
  where
    doParse ("Sue" : sue : props) = Just (sue, buildProperties props)
    doParse _ = Nothing

buildProperties :: [String] -> Map String Int
buildProperties = go empty
  where
    go acc (x : y : ys) = go (insert x (read y :: Int) acc) ys
    go acc _ = acc

main = do
  input <- readFile "app/input/day16.txt"
  return $ part2 input
