{-# LANGUAGE QuasiQuotes #-}

module Puzzle.Day23 where

import Data.Map (Map, adjust, fromList, insert, (!))
import qualified Text.RawString.QQ as QQ (r)

test :: String
test =
  [QQ.r|inc a
jio a, +2
tpl a
inc a|]

type Registers = Map Char Int

data Command = Half Char | Triple Char | Increment Char | Jump Int | JumpIfEven Char Int | JumpIfOne Char Int deriving (Show)

type Instructions = [Command]

data Computer = Computer
  { registers :: Registers,
    instructions :: Instructions,
    pointer :: Int,
    finalInstructionIndex :: Int,
    executed :: [Command]
  }
  deriving (Show)

part1 :: String -> Maybe Int
part1 = fmap ((! 'b') . registers . runInstructions . computer) . parse

part2 :: String -> Maybe Int
part2 = fmap ((! 'b') . registers . runInstructions . updateRegisterA . computer) . parse
  where
    updateRegisterA c@Computer {registers = rs} = c {registers = insert 'a' 1 rs}

computer :: Instructions -> Computer
computer i = Computer {registers = fromList [('a', 0), ('b', 0)], instructions = i, pointer = 0, finalInstructionIndex = length i - 1, executed = []}

executionComplete :: Computer -> Bool
executionComplete Computer {pointer = p, finalInstructionIndex = f} = p > f

runInstructions :: Computer -> Computer
runInstructions c@Computer {pointer = p, instructions = i, executed = e}
  | executionComplete c = c
  | otherwise = runInstructions . runInstruction cmd $ c {executed = cmd : e}
  where
    cmd = i !! p

runInstruction :: Command -> Computer -> Computer
runInstruction (Half r) c@Computer {registers = rs, pointer = p} = c {registers = adjust (`div` 2) r rs, pointer = p + 1}
runInstruction (Triple r) c@Computer {registers = rs, pointer = p} = c {registers = adjust (* 3) r rs, pointer = p + 1}
runInstruction (Increment r) c@Computer {registers = rs, pointer = p} = c {registers = adjust (+ 1) r rs, pointer = p + 1}
runInstruction (Jump o) c@Computer {pointer = p} = c {pointer = p + o}
runInstruction (JumpIfEven r o) c@Computer {registers = rs, pointer = p} = c {pointer = if even (rs ! r) then p + o else p + 1}
runInstruction (JumpIfOne r o) c@Computer {registers = rs, pointer = p} = c {pointer = if (rs ! r) == 1 then p + o else p + 1}

parse :: String -> Maybe Instructions
parse = mapM (doParse . words) . lines . filter (/= ',')
  where
    doParse ["hlf", [register]] = Just (Half register)
    doParse ["tpl", [register]] = Just (Triple register)
    doParse ["inc", [register]] = Just (Increment register)
    doParse ["jmp", offset] = Just (Jump (doRead offset))
    doParse ["jie", [register], offset] = Just (JumpIfEven register (doRead offset))
    doParse ["jio", [register], offset] = Just (JumpIfOne register (doRead offset))
    doParse _ = Nothing

doRead :: String -> Int
doRead ('+' : rest) = read rest :: Int
doRead rest = read rest :: Int

main :: IO (Maybe Int)
main = do
  input <- readFile "app/input/day23.txt"
  return $ part2 input
