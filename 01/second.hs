-- Advent of Code 2020 Day 1 Part 2 by KovaxG
-- https://adventofcode.com/2020/day/1

import qualified Util

main :: IO ()
main = Util.runProcessInts $ \entries -> show $ head [e1 * e2 * e3 | e1 <- entries, e2 <- entries, e3 <- entries, e1 + e2 + e3 == 2020]
