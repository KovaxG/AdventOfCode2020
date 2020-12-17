{-# LANGUAGE TupleSections #-}
module Util where


import qualified Data.Maybe as Maybe
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import           Data.Function (on)

type Coord = (Int, Int)

runProcess :: (String -> String) -> IO ()
runProcess process = readFile "input.txt" >>= putStrLn . process

runProcessInts :: ([Int] -> String) -> IO ()
runProcessInts process = runProcess (process . readIntsIgnoreBad)

runProcessMap :: Show a => (Char -> a) -> ((Map Coord a, Int, Int) -> String) -> IO ()
runProcessMap parse process =
  runProcess $ process . \input ->
    let width = length $ head $ lines input
        height = length $ lines input

        toCoord :: Int -> Coord
        toCoord x = (mod x width, div x height)

        stuff = Map.map parse $ Map.fromList $ concatMap (\(y, r) -> zip (map (,y) [0..]) r) $ zip [0..] $ lines input
    in (stuff, width, height)

showMap :: Show a => (Map Coord a, Int, Int) -> String
showMap (tiles, width, height) =
  unlines
  $ chunks width
  $ concatMap snd
  $ List.sortBy (on compare fst)
  $ map (Bifunctor.bimap toIndex show)
  $ Map.toList tiles
  where
    toIndex (x,y) = x + width * y

readIntsIgnoreBad :: String -> [Int]
readIntsIgnoreBad = Maybe.mapMaybe safeRead . lines

safeRead :: Read a => String -> Maybe a
safeRead = fmap fst . safeHead . reads

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:as) = Just as

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right a) = Just a
eitherToMaybe _ = Nothing

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

getAt :: [a] -> Int -> Maybe a
getAt as index
  | index >= 0 && index < length as = Just $ as !! index
  | otherwise = Nothing

xor :: Bool -> Bool -> Bool
xor a b = (a && not b) || (not a && b)

mapIf :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapIf p f = map (\a -> if p a then f a else a)

split :: Eq a => a -> [a] -> [[a]]
split c s =
  case rest of
    []     -> [chunk]
    _:rest -> chunk : split c rest
  where (chunk, rest) = break (==c) s

chunks :: Int -> [a] -> [[a]]
chunks n as
  | null rest = [chunk]
  | otherwise = chunk : chunks n rest
  where (chunk, rest) = splitAt n as


sliding :: Int -> Int -> [a] -> [[a]]
sliding step size as
  | length rest < size = [chunk]
  | otherwise = chunk : sliding step size rest
  where
    chunk = take size as
    rest = drop step as