-- Advent of Code 2020 Day 25 Part 2 by KovaxG
-- https://adventofcode.com/2020/day/25

main :: IO ()
main = readFile "input.txt" >>= putStrLn . process

process :: String -> String
process s = s
