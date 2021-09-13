module Puzzle.Day5 where

import Data.List.Utils
import Input.Day5

-- hasEnoungVowels :: String -> Bool
hasEnoungVowels = (> 2) . length . filter (\x -> elem x "aeiou")

-- isVowel = elem x "aeiou"

hasDupes :: String -> Bool
hasDupes (x : y : ys) = if x == y then True else hasDupes (y : ys)
hasDupes _ = False

doestHaveBadString :: String -> Bool
doestHaveBadString x = not . any (\y -> contains y x) $ ["ab", "cd", "pq", "xy"]

calculate :: [String -> Bool] -> String -> Int
calculate funs = length . filter (\x -> all (\y -> y x) funs) . lines

part1 :: String -> Int
part1 = calculate [hasEnoungVowels, hasDupes, doestHaveBadString]

hasSpreadDupes :: String -> Bool
hasSpreadDupes (x : y : z : zs) = if x == z then True else hasSpreadDupes (y : z : zs)
hasSpreadDupes _ = False

hasPairDupes :: String -> Bool
hasPairDupes (x : y : ys) = if contains [x, y] ys then True else hasPairDupes (y : ys)
hasPairDupes _ = False

part2 :: String -> Int
part2 = calculate [hasSpreadDupes, hasPairDupes]
