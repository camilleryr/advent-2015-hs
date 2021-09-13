{-# LANGUAGE QuasiQuotes #-}

module Puzzle.Day23 where

import qualified Text.RawString.QQ as QQ (r)

test :: String
test =
  [QQ.r|1
2
3
4
5
7
8
9
10
11|]

part1 :: String -> Int
part1 = solve 3

part2 :: String -> Int
part2 = solve 4

solve :: Int -> String -> Int
solve x i = minimum . map product $ potentialSolutions
  where
    potentialSolutions = getPotentialSolutions idealWeight all'
    idealWeight = div (sum all') x
    all' = parse i

getPotentialSolutions :: Int -> [Int] -> [[Int]]
getPotentialSolutions = go 1
  where
    go :: Int -> Int -> [Int] -> [[Int]]
    go length sum' all' =
      let matchedSubsequences = filter ((== sum') . sum) . subsequencesOfSize length $ all'
       in if null matchedSubsequences then go (length + 1) sum' all' else matchedSubsequences

parse :: String -> [Int]
parse = map doRead . lines
  where
    doRead x = read x :: Int

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs =
  let l = length xs
   in if n > l
        then []
        else subsequencesBySize xs !! (l - n)
  where
    subsequencesBySize [] = [[[]]]
    subsequencesBySize (x : xs) =
      let next = subsequencesBySize xs
       in zipWith
            (++)
            ([] : next)
            (map (map (x :)) next ++ [[]])

main :: IO Int
main = do
  input <- readFile "app/input/day24.txt"
  return $ part2 input
