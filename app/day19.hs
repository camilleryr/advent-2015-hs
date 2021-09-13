{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Puzzle.Day19 where

import Data.Function (on)
import Data.List (find, foldl', isPrefixOf, isSubsequenceOf, permutations, sortBy, subsequences)
import Data.Map.Strict (Map, empty, foldlWithKey', fromList, insertWith, member, singleton, size, union)
import Data.Text (Text, isInfixOf, length, pack, replace, unpack)
import Data.Tuple (swap)
import Text.RawString.QQ (r)

test =
  [r|H => HO
H => OH
O => HH|]

test2 =
  [r|e => H
e => O
H => HO
H => OH
O => HH|]

type Replacement = (String, String)

part2 :: String -> Maybe [Replacement] -> Maybe Int
part2 _ Nothing = Nothing
part2 start (Just replacers) = go 0 (pack start) . sortBy (flip compare `on` (Data.Text.length . fst)) . map packAndSwap $ replacers
  where
    go :: Int -> Text -> [(Text, Text)] -> Maybe Int
    go i "e" _ = Just i
    go i s r = go (i + 1) string' r
      where
        string' = replace matcher replacer s
        (matcher, replacer) = case find (\(x, _) -> x `isInfixOf` s) r of
          (Just x) -> x
          Nothing -> undefined

packAndSwap :: (String, String) -> (Text, Text)
packAndSwap (x, y) = (pack y, pack x)

part1 :: String -> Maybe [Replacement] -> Maybe Int
part1 x = fmap size . next x

next :: String -> Maybe [Replacement] -> Maybe (Map String Int)
next seq' = fmap (foldl' (folder "" seq') empty)
  where
    folder :: String -> String -> Map String Int -> Replacement -> Map String Int
    folder _ [] acc _ = acc
    folder h t@(t' : rest) acc x@(matcher, replacer) = folder (h ++ [t']) rest acc' x
      where
        acc' = if matcher `isPrefixOf` t then insertWith (+) (h ++ replacer ++ drop (Prelude.length matcher) t) 1 acc else acc

parseReplacements :: String -> Maybe [Replacement]
parseReplacements = mapM (doParse . words) . filter (isSubsequenceOf "=>") . lines
  where
    doParse [x, "=>", y] = Just (x, y)
    doParse x = error (show x)

parseSequence :: String -> String
parseSequence = last . lines

main = do
  input <- readFile "app/input/day19.txt"
  return $ part2 (parseSequence input) (parseReplacements input)
