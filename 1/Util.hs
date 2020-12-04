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
safeHead (a:as) = Just a
