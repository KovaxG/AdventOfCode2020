-- Advent of Code 2020 Day 3 Part 2 by KovaxG
-- https://adventofcode.com/2020/day/3

import qualified Util
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

main :: IO ()
main = Util.runProcess $ \input ->
    let area = populate input
        slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)]
    in show $ product $ map (countTrees area) slopes

type Coord = (Int, Int)

data Square = Empty | Tree deriving (Show, Eq)

data Area = Area (Map Coord Square) Int Int deriving (Show)

populate :: String -> Area
populate s = Area (Map.fromList indexedSquares) width height
  where
    rows = lines s
    width = length $ head rows
    height = length $ rows
    squares = map (map toSquare) rows

    indexedSquares :: [(Coord, Square)]
    indexedSquares = concatMap (\(y, r) -> zip (map (\x -> (x,y)) [0..]) r)
                   $ zip [0..] squares

    toSquare :: Char -> Square
    toSquare '.' = Empty
    toSquare '#' = Tree

getSquare :: Area -> Coord -> Maybe Square
getSquare (Area squares width height) (x, y) = Map.lookup (x', y) squares
  where x' = mod x width

countTrees :: Area -> (Int, Int) -> Int
countTrees area (xOff, yOff) =
  Util.count (==Tree)
  $ Maybe.catMaybes
  $ takeWhile (Maybe.isJust)
  $ map (getSquare area)
  $ iterate (\(x,y) -> (x + xOff, y + yOff)) (0,0)
