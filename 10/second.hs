-- Advent of Code 2020 Day 10 Part 2 by KovaxG
-- https://adventofcode.com/2020/day/10

main :: IO ()
main = readFile "input.txt" >>= putStrLn . process

process :: String -> String
process s = s
