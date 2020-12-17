-- Advent of Code 2020 Day 16 Part 1 by KovaxG
-- https://adventofcode.com/2020/day/16

import qualified Util
import           Text.Parsec ((<|>))
import qualified Text.Parsec as Parsec
import qualified Data.Maybe as Maybe

data Rule = R String  (Int, Int) (Int, Int) deriving (Show)
type Ticket = [Int]

readInput :: String -> [String]
readInput =
  Util.split '|'
  . unlines
  . map (\e -> if null e then "|" else e)
  .  lines

parseRule :: String -> Maybe Rule
parseRule = Util.eitherToMaybe . Parsec.parse rule ""
  where
    rule = do
      descr <- Parsec.many1 (Parsec.letter <|> Parsec.char ' ')
      Parsec.char ':'
      Parsec.spaces
      l11 <- read <$> Parsec.many1 Parsec.digit
      Parsec.char '-'
      l12 <- read <$> Parsec.many1 Parsec.digit
      Parsec.spaces
      Parsec.string "or"
      Parsec.spaces
      l21 <- read <$> Parsec.many1 Parsec.digit
      Parsec.char '-'
      l22 <- read <$> Parsec.many1 Parsec.digit
      return $ R descr (l11, l12) (l21, l22)

parseRules :: String -> [Rule]
parseRules = Maybe.mapMaybe parseRule . lines

parseMyTicket :: String -> Ticket
parseMyTicket = map read . Util.split ',' . last . lines

parseTickets :: String -> [Ticket]
parseTickets = map (map read . Util.split ',') . tail . tail . lines

isValid :: Rule -> Int -> Bool
isValid (R _ (s1, e1) (s2, e2)) n = s1 <= n && n <= e1 || s2 <= n && n <= e2

getInvalidFields :: [Rule] -> [Ticket] -> [Int]
getInvalidFields rs =
    concatMap (filter (\f -> not $ any (`isValid` f) rs))

main :: IO ()
main = Util.runProcess $ \input ->
  let [rules', your', others'] = readInput input
      rules = parseRules rules'
      your = parseMyTicket your'
      others = parseTickets others'
  in show $ sum $ getInvalidFields rules others



