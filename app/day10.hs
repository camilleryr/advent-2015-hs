{-# LANGUAGE QuasiQuotes #-}

module Puzzle.Day10 where

import Data.List (foldl', minimum, nub, permutations, sort)
import Data.List.Split (splitOn)
import Data.Map (Map, empty, insert, (!))
import Text.RawString.QQ (r)

test = "1"

input = "1113222113"

part1 :: Int -> String -> Int
part1 num string
  | num > 0 = part1 (num - 1) (next string)
  | otherwise = length string

-- part2 :: Int -> String -> Int
-- part2 int string = go int (length string)
--   where
--     go 0 total = total
--     go x total = go (x -1) (floor ((fromIntegral total :: Float) * 1.3))

next :: String -> String
next = go ""
  where
    go acc [] = reverse acc
    go acc ('1' : '1' : '1' : rest) = go ('1' : '3' : acc) rest
    go acc ('1' : '1' : rest) = go ('1' : '2' : acc) rest
    go acc ('1' : rest) = go ('1' : '1' : acc) rest
    go acc ('2' : '2' : '2' : rest) = go ('2' : '3' : acc) rest
    go acc ('2' : '2' : rest) = go ('2' : '2' : acc) rest
    go acc ('2' : rest) = go ('2' : '1' : acc) rest
    go acc ('3' : '3' : '3' : rest) = go ('3' : '3' : acc) rest
    go acc ('3' : '3' : rest) = go ('3' : '2' : acc) rest
    go acc ('3' : rest) = go ('3' : '1' : acc) rest

-- next :: String -> String
-- next = foldl' getNext "" . groupByLetter
--   where
--     getNext acc g@(x : _) = acc ++ show (length g) ++ [x]

-- groupByLetter :: String -> [String]
-- groupByLetter original = go original []
--   where
--     go [] acc = reverse acc
--     go all@(x : _) acc = go (dropWhile (== x) all) ((takeWhile (== x) all) : acc)
