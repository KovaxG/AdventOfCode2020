-- Advent of Code 2020 Day 22 Part 1 by KovaxG
-- https://adventofcode.com/2020/day/22

main :: IO ()
main = readFile "input.txt" >>= putStrLn . process

process :: String -> String
process s = s
