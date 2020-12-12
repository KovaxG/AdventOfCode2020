-- Advent of Code 2020 Day 11 Part 2 by KovaxG
-- https://adventofcode.com/2020/day/11

import           Util (Coord)
import qualified Util
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Bifunctor as Bifunctor

data Chair = Empty | Occupied deriving (Eq)

data State = State
  { chairs :: Map Coord Chair
  , width :: Int
  , height :: Int
  } deriving (Eq, Show)

instance Show Chair where
  show Empty = "L"
  show Occupied = "#"

toState :: (Map Coord Char, Int, Int) -> State
toState (c2c, w, h) = State
  { chairs = Map.map (\c -> if c == 'L' then Empty else Occupied) . Map.filter (/='.') $ c2c
  , width = w
  , height = h
  }

main :: IO ()
main = Util.runProcessMap id (logic . toState)

logic :: State -> String
logic = show . Util.count (==Occupied) . Map.elems . chairs . run

run :: State -> State
run s = let s' = update s in if s == s' then s' else run s'

update :: State -> State
update s = s { chairs = Map.mapWithKey updateSquare $ chairs s }
  where
    updateSquare :: Coord -> Chair -> Chair
    updateSquare coord chair = case chair of
      Empty -> if Util.count (==Occupied) adjacency == 0 then Occupied else Empty
      Occupied -> if Util.count (==Occupied) adjacency >= 5 then Empty else Occupied
      where adjacency = chairsInView s coord

chairsInView :: State -> Coord -> [Chair]
chairsInView s c =
  Maybe.catMaybes $ lookup <$>
    [ Bifunctor.bimap inc id
    , Bifunctor.bimap id inc
    , Bifunctor.bimap id dec
    , Bifunctor.bimap dec id
    , Bifunctor.bimap dec dec
    , Bifunctor.bimap inc inc
    , Bifunctor.bimap inc dec
    , Bifunctor.bimap dec inc
    ]
  where
    lookup = sight s c
    inc x = x + 1
    dec x = x - 1

sight :: State -> Coord -> (Coord -> Coord) -> Maybe Chair
sight s p f =
  maybe (if inBounds next then sight s next f else Nothing) Just
  $ Map.lookup next
  $ chairs s
  where
    next = f p
    inBounds (x,y) = (x >= 0 && x <= width s) && (y >= 0 && y <= height s)
