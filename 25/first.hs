-- Advent of Code 2020 Day 25 Part 1 by KovaxG
-- https://adventofcode.com/2020/day/25

{-# LANGUAGE BangPatterns #-}

import qualified Util

main :: IO ()
main = Util.runProcessInts $ \[pk1, pk2] -> show $ transform pk2 (reverseEngineer pk1)

reverseEngineer :: Int -> Int
reverseEngineer pk = go 1 0
  where
    go :: Int -> Int -> Int
    go !n !l
      | n == pk   = l
      | otherwise = go (n * 7 `mod` 20201227) (l+1)

transform :: Int -> Int -> Int
transform source loop = go 1 0
  where
    go :: Int -> Int -> Int
    go !s !l
      | l == loop = s
      | otherwise = go (s * source `mod` 20201227) (l+1)
