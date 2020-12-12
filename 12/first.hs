-- Advent of Code 2020 Day 12 Part 1 by KovaxG
-- https://adventofcode.com/2020/day/12

import qualified Util
import qualified Data.Maybe as Maybe

data Dir = N | S | E | W | L | R | F deriving (Show, Read)
data Action = Action Dir Int deriving (Show)

type State = (Int, (Int, Int))

parseAction :: String -> Maybe Action
parseAction [] = Nothing
parseAction (c:nrStr) = do
  nr <- Util.safeRead nrStr
  dir <- Util.safeRead [c]
  return $ Action dir nr

main :: IO ()
main = Util.runProcess $ show . manhattanDist . foldl move (0, (0,0)) . Maybe.mapMaybe parseAction . lines

move :: State -> Action -> State
move s@(a, (x,y)) (Action dir d) = case dir of
  W -> (a, (x - d, y))
  E -> (a, (x + d, y))
  N -> (a, (x, y + d))
  S -> (a, (x, y - d))
  L -> (a + d, (x, y))
  R -> (a - d, (x, y))
  F -> (a, forward)
  where
    forward :: (Int, Int)
    forward = (x + round (cos af * df), y + round (sin af * df))

    af = deg2rad $ fromIntegral a
    df = fromIntegral d

deg2rad :: Double -> Double
deg2rad d = (d * 2 * pi) / 360.0

manhattanDist :: State -> Int
manhattanDist (_, (x, y)) = abs x + abs y