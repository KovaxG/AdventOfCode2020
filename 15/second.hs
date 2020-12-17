-- Advent of Code 2020 Day 15 Part 2 by KovaxG
-- https://adventofcode.com/2020/day/15

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.List as List
import qualified Debug.Trace as Debug

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
    newNr = maybe 0 (index -) $ Map.lookup prevNr past

initState :: [Nr] -> State
initState (x:xs) = State { past = Map.fromList $ zip (reverse xs) [1 ..], index = 1 + length xs, prevNr = x }

main :: IO ()
main = print $ calculate [0,3,6] 30000000

calculate :: [Nr] -> Int -> Nr
calculate !xs !num = prevNr . go . initState . reverse $ xs
  where
    go :: State -> State
    go s@State{index}
      | index == num = s
      | otherwise = Debug.traceShow (length $ past s) $ go (tick s)
