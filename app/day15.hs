{-# LANGUAGE QuasiQuotes #-}

module Puzzle.Day15 where

import Data.Function (on)
import Data.List (maximumBy)
import Data.List.Split (splitOn)
import Text.RawString.QQ (r)

test :: String
test =
  [r|Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3|]

input :: String
input =
  [r|Sugar: capacity 3, durability 0, flavor 0, texture -3, calories 2
Sprinkles: capacity -3, durability 3, flavor 0, texture 0, calories 9
Candy: capacity -1, durability 0, flavor 4, texture 0, calories 1
Chocolate: capacity 0, durability 0, flavor -2, texture 2, calories 8|]

data Ingredient = Ingredient String Int Int Int Int Int deriving (Show)

possibilities :: [(Int, Int, Int, Int)]
possibilities = [(w, x, y, z) | w <- [0 .. 100], x <- [0 .. (100 - w)], y <- [0 .. (100 - w - x)], z <- [0 .. (100 - w - x - y)], w + x + y + z == 100]

part1 :: String -> Maybe Int
part1 = fmap (maximum . map getScore . expand) . parse

part2 :: String -> Maybe Int
part2 = fmap (maximum . map getScore . filter (hasCallories 500) . expand) . parse

hasCallories :: Int -> (Ingredient, Ingredient, Ingredient, Ingredient) -> Bool
hasCallories x (Ingredient _ _ _ _ _ c, Ingredient _ _ _ _ _ c', Ingredient _ _ _ _ _ c'', Ingredient _ _ _ _ _ c''') =
  (c + c' + c'' + c''') == x

getScore :: (Ingredient, Ingredient, Ingredient, Ingredient) -> Int
getScore
  (Ingredient _ w x y z _, Ingredient _ w' x' y' z' _, Ingredient _ w'' x'' y'' z'' _, Ingredient _ w''' x''' y''' z''' _) =
    max (w + w' + w'' + w''') 0 * max (x + x' + x'' + x''') 0 * max (y + y' + y'' + y''') 0 * max (z + z' + z'' + z''') 0

expand :: [Ingredient] -> [(Ingredient, Ingredient, Ingredient, Ingredient)]
expand [w, x, y, z] = foldl folder [] possibilities
  where
    folder acc (w', x', y', z') = (mult w' w, mult x' x, mult y' y, mult z' z) : acc
expand _ = undefined

mult :: Int -> Ingredient -> Ingredient
mult x (Ingredient name capacity durability flavor texture calories) = Ingredient name (capacity * x) (durability * x) (flavor * x) (texture * x) (calories * x)

parse :: String -> Maybe [Ingredient]
parse = mapM (doParse . words) . splitOn "\n" . filter (not . flip elem ":,")
  where
    doParse [name, "capacity", capacity, "durability", durability, "flavor", flavor, "texture", texture, "calories", calories] = Just (Ingredient name (read capacity :: Int) (read durability :: Int) (read flavor :: Int) (read texture :: Int) (read calories :: Int))
    doParse _ = Nothing
