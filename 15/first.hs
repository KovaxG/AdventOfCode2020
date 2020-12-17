-- Advent of Code 2020 Day 15 Part 1 by KovaxG
-- https://adventofcode.com/2020/day/15

{-# LANGUAGE NamedFieldPuns #-}

import           Data.Map (Map)
import qualified Data.Map as Map

type Nr = Int
type Index = Int

data State = State
  { past :: Map Nr Index
  , prevNr :: Nr
  , index :: Index
  } deriving (Show)

tick :: State -> State
tick s@State{past, prevNr, index} =
  s { prevNr = newNr
    , index = newIndex
    , past = Map.insert prevNr index past
    }
  where
    newIndex = index + 1
    newNr = maybe 0 (index -)
           $ Map.lookup prevNr past

initState :: [Nr] -> State
initState (x:xs) = State { past = Map.fromList $ zip (reverse xs) [1 ..], index = 1 + length xs, prevNr = x }

main :: IO ()
main = print $ calculate [5,1,9,18,13,8,0]

calculate :: [Nr] -> Nr
calculate xs = last . map prevNr . take (2020 - length xs + 1) . iterate tick . initState . reverse $ xs