module Util where

import qualified Data.Maybe as Maybe

runProcess :: (String -> String) -> IO ()
runProcess process = readFile "input.txt" >>= putStrLn . process

runProcessInts :: ([Int] -> String) -> IO ()
runProcessInts process = runProcess (process . readIntsIgnoreBad)

readIntsIgnoreBad :: String -> [Int]
readIntsIgnoreBad = Maybe.catMaybes . map safeRead . lines

safeRead :: Read a => String -> Maybe a
safeRead = fmap fst . safeHead . reads

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (a:_) = Just a

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
