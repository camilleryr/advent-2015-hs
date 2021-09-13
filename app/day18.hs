{-# LANGUAGE QuasiQuotes #-}

module Puzzle.Day18 where

import Data.Function (on)
import Data.List (sortBy, subsequences)
import Data.Map (Map, empty, foldWithKey, fromList, insert, member, size, union)
import Text.RawString.QQ (r)

test :: String
test =
  [r|.#.#.#
...##.
#....#
..#...
#.#..#
####..|]

part1 :: Int -> Int -> String -> Int
part1 s i = size . mutate s i id . parse

part2 :: Int -> Int -> String -> Int
part2 s i = size . mutate s i (\x -> x `union` fromList [((0, 0), '#'), ((0, s), '#'), ((s, 0), '#'), ((s, s), '#')]) . parse

mutate :: Int -> Int -> (Map (Int, Int) Char -> Map (Int, Int) Char) -> Map (Int, Int) Char -> Map (Int, Int) Char
mutate s x f y = go 0 y
  where
    go i state = if i == x then f state else go (i + 1) (doMutate . f $ state)
    doMutate state' = foldl (folder state') empty [(x, y) | x <- [0 .. s], y <- [0 .. s]]
    folder state'' acc' cord
      | member cord state'' = if countNeighbors state'' cord `elem` [2, 3] then insert cord '#' acc' else acc'
      | otherwise = if countNeighbors state'' cord == 3 then insert cord '#' acc' else acc'

countNeighbors :: Map (Int, Int) Char -> (Int, Int) -> Int
countNeighbors state (x, y) = foldl (\acc el -> if member el state then acc + 1 else acc) 0 [(x', y') | x' <- [x -1 .. x + 1], y' <- [y -1 .. y + 1], (x', y') /= (x, y)]

parse :: String -> Map (Int, Int) Char
parse = snd . foldl build (0, empty) . lines
  where
    build (y, acc) line = (y + 1, snd . foldl build' ((0, y), acc) $ line)
    build' (cord@(x, y), acc) '#' = ((x + 1, y), insert cord '#' acc)
    build' ((x, y), acc) _ = ((x + 1, y), acc)

main = do
  input <- readFile "app/input/day18.txt"
  return $ part2 99 100 input
