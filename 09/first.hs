-- Advent of Code 2020 Day 9 Part 1 by KovaxG
-- https://adventofcode.com/2020/day/9

import qualified Util
import qualified Data.List as List

main :: IO ()
main = Util.runProcessInts $
  show
  . fmap head
  . List.find (not . isFine)
  . Util.sliding 1 26
  . reverse

-- First element is checked
isFine :: [Int] -> Bool
isFine (h:rest) = not $ null [(x,y) | x <- rest, y <- rest, x + y == h]

