module Puzzle.Day20 where

import Data.List (findIndex, nub)
import GHC.Float (float2Int, int2Float)

input :: Int
input = 33100000

part1 :: Int -> Maybe Int
part1 x = findIndex (>= x) . map (calculatePresents . getDivisors) $ [0 ..]

part2 :: Int -> Int -> Maybe Int
part2 n x = fmap (+ n) . findIndex (>= x) . map (\y -> calculatePresents' . filter (\z -> div y z <= 50) . getDivisors $ y) $ [n ..]

getDivisors :: Int -> [Int]
getDivisors x = nub [n | y <- [1 .. sqrtOfX], mod x y == 0, n <- [y, div x y]]
  where
    sqrtOfX = float2Int . sqrt . int2Float $ x

calculatePresents :: [Int] -> Int
calculatePresents = (10 *) . sum

calculatePresents' :: [Int] -> Int
calculatePresents' = (11 *) . sum
