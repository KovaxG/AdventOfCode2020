-- Advent of Code 2020 Day 5 Part 1 by KovaxG
-- https://adventofcode.com/2020/day/5

import qualified Util

main :: IO ()
main = Util.runProcess $ show . maximum . map scanID . lines

scanID :: String -> Int
scanID s = row * 8 + col
  where
    (rowStr, colStr) = splitAt 7 s
    row = sum $ zipWith (*) (iterate (*2) 1) (reverse $ map (\c -> if c == 'B' then 1 else 0) rowStr)
    col = sum $ zipWith (*) (iterate (*2) 1) (reverse $ map (\c -> if c == 'R' then 1 else 0) colStr)
