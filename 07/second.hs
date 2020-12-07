-- Advent of Code 2020 Day 7 Part 2 by KovaxG
-- https://adventofcode.com/2020/day/7

import qualified Util
import           Text.Parsec ((<|>))
import qualified Text.Parsec as Parsec
import qualified Data.Maybe as Maybe
import qualified Data.List as List

main :: IO ()
main = Util.runProcess $ \input ->
  let rules = Maybe.catMaybes $ map parseRule $ lines input
  in show $ countBags rules "shiny gold" - 1

type Label = String
data Rule = Contains Label [(Int, Label)] deriving (Show)

countBags :: [Rule] -> Label -> Int
countBags rules bag =
  Maybe.fromMaybe 0
  $ fmap (bagNr rules)
  $ List.find (\(Contains n _) -> n == bag)
  $ rules

bagNr :: [Rule] -> Rule -> Int
bagNr rules (Contains name nrBags)
  | null nrBags = 1
  | otherwise =
    (+1)
    $ sum
    $ map (\(nr, bag) -> Maybe.fromMaybe 0 $ fmap (\r -> nr * bagNr rules r) $ toRule bag)
    $ nrBags
    where
      toRule :: Label -> Maybe Rule
      toRule bag = List.find (\(Contains n _) -> n == bag) rules

allBags :: [Rule] -> [Label]
allBags = map $ \(Contains name _) -> name

parseRule :: String -> Maybe Rule
parseRule = Util.eitherToMaybe . Parsec.parse rule ""
  where
    rule = do
      attr <- Parsec.many1 Parsec.letter
      Parsec.spaces
      color <- Parsec.many1 Parsec.letter
      Parsec.spaces
      Parsec.string "bags contain"
      Parsec.spaces
      bags <- bags
      Parsec.char '.'
      return $ Contains (unwords [attr, color]) bags

    bags = Parsec.try noBags <|> Parsec.try someBags

    noBags = do
      Parsec.string "no other bags"
      return []

    someBags = Parsec.sepBy bag (Parsec.char ',' >> Parsec.spaces)

    bag = do
      nr <- read <$> Parsec.many1 Parsec.digit
      Parsec.spaces
      attr <- Parsec.many1 Parsec.letter
      Parsec.spaces
      color <- Parsec.many1 Parsec.letter
      Parsec.spaces
      Parsec.try (Parsec.string "bags") <|> Parsec.try (Parsec.string "bag")
      return (nr, unwords [attr, color])
