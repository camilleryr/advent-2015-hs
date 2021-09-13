{-# LANGUAGE QuasiQuotes #-}

module Puzzle.Day17 where

import Data.Function (on)
import Data.List (sortBy, subsequences)
import Text.RawString.QQ (r)

part1 :: String -> Int
part1 = length . filter (\x -> sum x == 150) . subsequences . parse

part2 :: String -> Int
part2 = length . matchesLengthOfHead . sortBy (compare `on` length) . filter (\x -> sum x == 150) . subsequences . parse
  where
    matchesLengthOfHead xs@(x : _) = filter (\x' -> length x' == length x) xs
    matchesLengthOfHead _ = undefined

parse :: String -> [Int]
parse = map (\x -> read x :: Int) . lines

main = do
  input <- readFile "app/input/day17.txt"
  return $ part2 input
