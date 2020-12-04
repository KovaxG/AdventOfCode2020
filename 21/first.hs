-- Advent of Code 2020 Day 21 Part 1 by KovaxG
-- https://adventofcode.com/2020/day/21

main :: IO ()
main = readFile "input.txt" >>= putStrLn . process

process :: String -> String
process s = s
