-- Advent of Code 2020 Day 6 Part 2 by KovaxG
-- https://adventofcode.com/2020/day/6

import qualified Util
import qualified Data.List as List

main :: IO ()
main = Util.runProcess $
  show
  . sum
  . map (length . foldl List.intersect ['a' .. 'z'] . words)
  . lines
  . unwords
  . Util.mapIf (=="") (const "\n")
  . lines
