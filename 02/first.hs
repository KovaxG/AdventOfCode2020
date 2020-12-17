-- Advent of Code 2020 Day 2 Part 1 by KovaxG
-- https://adventofcode.com/2020/day/2

import qualified Util
import qualified Data.Maybe as Maybe
import qualified Text.Parsec as Parsec

main :: IO ()
main = Util.runProcess $ show . Util.count isCorrect . Maybe.mapMaybe parseLine . lines

data Line = Line Int Int Char String deriving (Show)

isCorrect :: Line -> Bool
isCorrect (Line min max c pswd) = min <= nr && nr <= max
  where nr = Util.count (==c) pswd

parseLine :: String -> Maybe Line
parseLine = Util.eitherToMaybe . Parsec.parse line ""
  where
    line = do
      min <- read <$> Parsec.many1 Parsec.digit
      Parsec.char '-'
      max <- read <$> Parsec.many1 Parsec.digit
      Parsec.spaces
      c <- Parsec.letter
      Parsec.char ':'
      Parsec.spaces
      pswd <- Parsec.many1 Parsec.letter
      return $ Line min max c pswd
