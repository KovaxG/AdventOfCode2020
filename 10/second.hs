-- Advent of Code 2020 Day 10 Part 2 by KovaxG
-- https://adventofcode.com/2020/day/10

import qualified Util
import qualified Data.List as List

main :: IO ()
main = Util.runProcessInts $ \adapters ->
  show
  $ product
  $ map (formula . length)
  $ Util.split 3
  $ map diff
  $ Util.sliding 1 2
  $ List.sort
  $ (:) (maximum adapters + 3)
  $ (:) 0
  $ adapters

diff :: [Int] -> Int
diff [a, b] = b - a

formula :: Int -> Int
formula n = case n of
  0 -> 1
  1 -> 1
  2 -> 2
  3 -> 4
  4 -> 7
