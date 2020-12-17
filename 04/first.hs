-- Advent of Code 2020 Day 4 Part 1 by KovaxG
-- https://adventofcode.com/2020/day/4

import qualified Util
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

main :: IO ()
main = Util.runProcess $ show . Util.count id . map isValid . getPassports

getPassports :: String -> [Map String String]
getPassports =
  map (Map.fromList . map toTuple . words)
  . Util.split '|'
  .  unwords
  . Util.mapIf (=="") (const "|")
  . lines

toTuple :: String -> (String, String)
toTuple s = (take 3 s, drop 4 s)

isValid :: Map String String -> Bool
isValid pass =
    all (\k -> Maybe.isJust $ Map.lookup k pass)
        ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
