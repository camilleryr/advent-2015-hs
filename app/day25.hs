module Puzzle.Day23 where

import Data.List (foldl', unfoldr)

origin :: (Int, Int)
origin = (1, 1)

start :: (Int, (Int, Int))
start = (20151125, origin)

final :: (Int, Int)
final = (3029, 2947)

part1 :: (Int, Int) -> Int
part1 end = foldl' (\acc p -> if p == origin then 20151125 else nextVal acc) 0 (points end)

-- part1 :: (Int, Int) -> (Int, (Int, Int)) -> Int
-- part1 p@(x, y) (z, p'@(x', y'))
--   | x == x' && y == y' = z
--   | otherwise =
--     let z' = (nextVal z, nextPoint p')
--      in z' `seq` part1 p z'

points :: (Int, Int) -> [(Int, Int)]
points end = unfoldr (\b -> if b == end' then Nothing else Just (b, nextPoint b)) origin
  where
    end' = nextPoint end

nextVal :: Int -> Int
nextVal x = rem (x * 252533) 33554393

nextPoint :: (Int, Int) -> (Int, Int)
nextPoint (x, 1) = (1, x + 1)
nextPoint (x, y) = (x + 1, y - 1)
