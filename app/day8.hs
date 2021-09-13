{-# LANGUAGE QuasiQuotes #-}

module Puzzle.Day8 where

import Data.Char (isHexDigit)
import Data.List.Split (splitOn)
import Input.Day8
import Text.RawString.QQ (r)

test1 =
  [r|""
"abc"
"aaa\"aaa"
"\x27"|]

part1 :: String -> Int
part1 = calculateFinal . foldl sumTuples (0, 0) . map getLengths . parse

part2 :: String -> Int
part2 = calculateFinal . foldl sumTuples (0, 0) . map getLengths' . parse

calculateFinal :: (Int, Int) -> Int
calculateFinal (x, y) = x - y

sumTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumTuples (x, y) (x', y') = (x + x', y + y')

getLengths :: String -> (Int, Int)
getLengths x = (length x, getInMemeory x)

getLengths' :: String -> (Int, Int)
getLengths' x = (length (show x), length x)

getInMemeory :: String -> Int
getInMemeory x = go x 0
  where
    go [] acc = acc - 2
    go ('\\' : '\\' : rest) acc = go rest (acc + 1)
    go ('\\' : '\"' : rest) acc = go rest (acc + 1)
    go ('\\' : 'x' : y : z : rest) acc = if all isHexDigit [y, z] then go rest (acc + 1) else go (y : z : rest) (acc + 1)
    go (_ : rest) acc = go rest (acc + 1)

parse :: String -> [String]
parse = splitOn "\n"
