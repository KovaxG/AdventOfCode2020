-- Advent of Code 2020 Day 24 Part 2 by KovaxG
-- https://adventofcode.com/2020/day/24

main :: IO ()
main = readFile "input.txt" >>= putStrLn . process

process :: String -> String
process s = s
