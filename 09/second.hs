-- Advent of Code 2020 Day 9 Part 2 by KovaxG
-- https://adventofcode.com/2020/day/9

import qualified Util
import qualified Data.List as List

target = 22406676

main :: IO ()
main = Util.runProcessInts $ show . search []

search :: [Int] -> [Int] -> Int
search acc list@(next:rest)
  | sum acc == target = minimum acc + maximum acc
  | sum acc < target = search (acc ++ [next]) rest
  | sum acc > target = search (tail acc) list