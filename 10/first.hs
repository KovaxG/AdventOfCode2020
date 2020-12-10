-- Advent of Code 2020 Day 10 Part 1 by KovaxG
-- https://adventofcode.com/2020/day/10

import qualified Util
import qualified Data.List as List

main :: IO ()
main = Util.runProcessInts $ \adapters ->
  let diffs = map diff $ Util.sliding 1 2 $ List.sort $ maximum adapters + 3 : 0 : adapters
  in show $ Util.count (==1) diffs * Util.count(==3) diffs

diff :: [Int] -> Int
diff [a, b] = b - a