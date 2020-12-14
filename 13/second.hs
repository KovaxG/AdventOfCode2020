-- Advent of Code 2020 Day 13 Part 2 by KovaxG
-- https://adventofcode.com/2020/day/13

import qualified Util
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Function (on)

parseInput :: String -> [(Int, Int)]
parseInput s = case lines s of
  [_, bussStr] -> Maybe.mapMaybe (traverse Util.safeRead) $ zip [0 ..] $ Util.split ',' bussStr

main :: IO ()
main = Util.runProcess $ show . parseInput

logic :: (Double, [Double]) -> Int
logic (t, bs) =
  let (nr, wait) = List.maximumBy (on compare snd)
                 $ map (\b -> (b, round t - round b * ceiling(t / b))) bs
  in abs $ round nr * wait
