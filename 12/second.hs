-- Advent of Code 2020 Day 12 Part 2 by KovaxG
-- https://adventofcode.com/2020/day/12

import           Util (Coord)
import qualified Util
import qualified Data.Maybe as Maybe

data Dir = N | S | E | W | L | R | F deriving (Show, Read)
data Action = Action Dir Int deriving (Show)

type State = (Coord, Coord)

parseAction :: String -> Maybe Action
parseAction [] = Nothing
parseAction (c:nrStr) = do
  nr <- Util.safeRead nrStr
  dir <- Util.safeRead [c]
  return $ Action dir nr

main :: IO ()
main = Util.runProcess $
  show
  . manhattanDist
  . foldl move ((10, 1), (0,0))
  . Maybe.mapMaybe parseAction
  . lines

move :: State -> Action -> State
move s@((wx, wy), (sx, sy)) (Action dir d) = case dir of
  W -> ((wx - d, wy), (sx, sy))
  E -> ((wx + d, wy), (sx, sy))
  N -> ((wx, wy + d), (sx, sy))
  S -> ((wx, wy - d), (sx, sy))
  L -> (rotate (mod d 360) (wx, wy), (sx, sy))
  R -> (rotate (mod (-d) 360) (wx, wy), (sx, sy))
  F -> ((wx, wy), (sx + wx * d, sy + wy * d))

rotate :: Int -> Coord -> Coord
rotate a (x, y) = case a of
  0   -> (x, y)
  90  -> (-y, x)
  180 -> (-x, -y)
  270 -> (y, -x)

manhattanDist :: State -> Int
manhattanDist (_, (x, y)) = abs x + abs y
