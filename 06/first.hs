-- Advent of Code 2020 Day 6 Part 1 by KovaxG
-- https://adventofcode.com/2020/day/6

import qualified Util
import qualified Data.List as List

main :: IO ()
main = Util.runProcess $
  show
  . sum
  . map (length . List.nub . filter (/=' '))
  . lines
  . unwords
  . Util.mapIf (=="") (const "\n")
  . lines
