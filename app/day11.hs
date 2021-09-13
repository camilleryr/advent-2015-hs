module Puzzle.Day11 where

import Data.Char (chr, ord)
import Data.Foldable (foldr')

test1 = "hijklmmn"

test2 = "abbceffg"

test3 = "abbcegjk"

input = "vzbxkghb"

part1 :: String -> String
part1 x = if all (\y -> y z) [not . hasBadLetters, hasSequence, hasDupes 2] then z else part1 z
  where
    z = increment x

-- where
--   z = increment x

increment :: String -> String
increment s = snd $ foldr' folder (True, "") s
  where
    folder 'z' (True, acc) = (True, 'a' : acc)
    folder x (True, acc) = (False, chr (ord x + 1) : acc)
    folder x (False, acc) = (False, x : acc)

hasSequence :: String -> Bool
hasSequence (x : y : z : zs) = (ord y - ord x == 1 && ord z - ord y == 1) || hasSequence (y : z : zs)
hasSequence _ = False

hasBadLetters :: String -> Bool
hasBadLetters x = elem 'i' x || elem 'o' x || elem 'l' x

hasDupes :: Integer -> String -> Bool
hasDupes 0 _ = True
hasDupes i (x : y : ys) = if x == y then hasDupes (i - 1) ys else hasDupes i (y : ys)
hasDupes _ _ = False
