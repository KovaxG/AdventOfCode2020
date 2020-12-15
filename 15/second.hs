-- Advent of Code 2020 Day 15 Part 2 by KovaxG
-- https://adventofcode.com/2020/day/15

{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}

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
    newNr = maybe 0 (\old -> index - old)
           $ Map.lookup prevNr past

initState :: [Nr] -> State
initState (x:xs) = State { past = Map.fromList $ zip (reverse xs) [1 ..], index = 1 + length xs, prevNr = x }

main :: IO ()
main = putStrLn $ show $ calculate [5,1,9,18,13,8,0]

calculate :: [Nr] -> Nr
calculate xs = prevNr . go . initState . reverse $ xs
  where
    go :: State -> State
    go !s@State{index}
      | index == 30000000 = s
      | otherwise = go (tick s)