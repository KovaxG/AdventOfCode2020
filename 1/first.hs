-- Advent of Code 2020 Day 1 Part 1 by KovaxG
-- https://adventofcode.com/2020/day/1

import qualified Util

main :: IO ()
main = Util.runProcessInts $ \entries -> show $ head [e1 * e2 | e1 <- entries, e2 <- entries, e1 + e2 == 2020]
