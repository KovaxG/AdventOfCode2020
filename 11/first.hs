-- Advent of Code 2020 Day 11 Part 1 by KovaxG
-- https://adventofcode.com/2020/day/11

import           Util (Coord)
import qualified Util
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

data Chair = Empty | Occupied deriving (Eq)

type State = Map Coord Chair

instance Show Chair where
  show Empty = "L"
  show Occupied = "#"

toState :: Map Coord Char -> Map Coord Chair
toState = Map.map (\c -> if c == 'L' then Empty else Occupied) . Map.filter (/='.')

main :: IO ()
main = Util.runProcessMap id logic

logic :: (Map Coord Char, Int, Int) -> String
logic (c2s, _, _) = show $ Util.count (==Occupied) $ Map.elems $ run $ toState c2s

run :: State -> State
run s = let s' = update s in if s == s' then s' else run s'

update :: State -> State
update c2c = Map.mapWithKey updateSquare c2c
  where
    updateSquare :: Coord -> Chair -> Chair
    updateSquare c s = case s of
      Empty -> if Util.count (==Occupied) adjacency == 0 then Occupied else Empty
      Occupied -> if Util.count (==Occupied) adjacency >= 4 then Empty else Occupied
      where adjacency = adjacentChairs c2c c

adjacentChairs :: Map Coord Chair -> Coord -> [Chair]
adjacentChairs c2c (cx, cy) =
  Maybe.catMaybes $ flip Map.lookup c2c <$> [(x,y) | x <- [cx-1 .. cx+1], y <- [cy-1 .. cy+1], not (cx == x && cy == y)]
