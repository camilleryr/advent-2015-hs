{-# LANGUAGE QuasiQuotes #-}

module Puzzle.Day7 where

import Data.Bits
import Data.Either (isRight)
import Data.List (foldl')
import Data.List.Split
import Data.Map
import Input.Day7
import Text.RawString.QQ

test =
  [r|123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i|]

data OperationType = IS | NOT | AND | OR | LSHIFT | RSHIFT deriving (Show, Eq)

-- data Operation a b = OperationType a b deriving (Show, Eq)

type IntOrVar = Either String Int

part1 :: String -> String -> Maybe Int
part1 key = fmap (\x -> buildCircuit x ! key) . parse doRead

part2 :: String -> String -> Maybe Int
part2 key = fmap (\x -> buildCircuit x ! key) . parse doRead'

buildCircuit :: [(OperationType, [IntOrVar], String)] -> Map String Int
buildCircuit operations = go (empty, operations)
  where
    go (values, []) = values
    go (values, operations) = go (Data.List.foldl' folder (values, []) operations)
    folder (values, ops) (optype, args, key) = if all isRight args then (insert key (normalize (perform optype args)) values, ops) else (values, (optype, fmap (maybeTransorm values) args, key) : ops)
    maybeTransorm values x@(Left y) = if member y values then Right (values ! y) else x
    maybeTransorm values x = x

normalize :: Int -> Int
normalize x = if x >= 0 then x else 65536 + x

perform :: OperationType -> [IntOrVar] -> Int
perform IS [Right x] = x
perform NOT [Right x] = complement x
perform AND [Right x, Right y] = (.&.) x y
perform OR [Right x, Right y] = (.|.) x y
perform LSHIFT [Right x, Right y] = shiftL x y
perform RSHIFT [Right x, Right y] = shiftR x y
perform _ _ = undefined

parse :: (String -> IntOrVar) -> String -> Maybe [(OperationType, [IntOrVar], String)]
parse reader = mapM (doParse . splitOn " ") . splitOn "\n"
  where
    doParse [x, "->", y] = Just (IS, [reader x], y)
    doParse ["NOT", x, "->", y] = Just (NOT, [reader x], y)
    doParse [x, "AND", y, "->", z] = Just (AND, [reader x, reader y], z)
    doParse [x, "OR", y, "->", z] = Just (OR, [reader x, reader y], z)
    doParse [x, "LSHIFT", y, "->", z] = Just (LSHIFT, [reader x, reader y], z)
    doParse [x, "RSHIFT", y, "->", z] = Just (RSHIFT, [reader x, reader y], z)
    doParse _ = Nothing

doRead :: String -> IntOrVar
doRead x = if all (`elem` "1234567890") x then Right (read x :: Int) else Left x

doRead' :: String -> IntOrVar
doRead' "b" = Right 3176
doRead' x = doRead x
