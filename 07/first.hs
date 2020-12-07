-- Advent of Code 2020 Day 7 Part 1 by KovaxG
-- https://adventofcode.com/2020/day/7

import qualified Util
import           Text.Parsec ((<|>))
import qualified Text.Parsec as Parsec
import qualified Data.Maybe as Maybe
import qualified Data.List as List

main :: IO ()
main = Util.runProcess $ \input ->
  let rules = Maybe.catMaybes $ map parseRule $ lines input
      bags = allBags rules
  in show $ Util.count id $ map (canContain rules "shiny gold") bags


type Label = String
data Rule = Contains Label [(Int, Label)] deriving (Show)

canContain :: [Rule] -> Label -> Label -> Bool
canContain rules bag cbag =
    Maybe.fromMaybe False
    $ fmap (\r -> elemOf rules r bag)
    $ List.find (\(Contains n _) -> n == cbag)
    $ rules

elemOf :: [Rule] -> Rule -> Label -> Bool
elemOf rules (Contains name nrBags) bag =
    if null bags || name == bag
    then False
    else elem bag bags || any (\b -> canContain rules bag b) bags
    where bags = map snd nrBags

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
