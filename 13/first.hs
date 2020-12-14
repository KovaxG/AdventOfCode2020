-- Advent of Code 2020 Day 13 Part 1 by KovaxG
-- https://adventofcode.com/2020/day/13

import qualified Util
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Function (on)

parseInput :: String -> (Double, [Double])
parseInput s = case lines s of
  [nrStr, bussStr] ->
    let nr = read nrStr
        busses = Maybe.mapMaybe Util.safeRead $ Util.split ',' bussStr
    in (nr, busses)

main :: IO ()
main = Util.runProcess $ show . logic . parseInput

logic :: (Double, [Double]) -> Int
logic (t, bs) =
  let (nr, wait) = List.maximumBy (on compare snd)
                 $ map (\b -> (b, round t - round b * ceiling(t / b))) bs
  in abs $ round nr * wait
