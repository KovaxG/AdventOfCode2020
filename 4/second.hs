-- Advent of Code 2020 Day 4 Part 2 by KovaxG
-- https://adventofcode.com/2020/day/4

import qualified Util
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Char as Char

type Passport = Map String String

main :: IO ()
main = Util.runProcess $
  show
  . Util.count id
  . map (Maybe.isJust . validate)
  . getPassports

getPassports :: String -> [Passport]
getPassports =
  map (Map.fromList . map toTuple . words)
  . Util.split '|'
  .  unwords
  . Util.mapIf (=="") (\_ -> "|")
  . lines

toTuple :: String -> (String, String)
toTuple s = (take 3 s, drop 4 s)

validate :: Passport -> Maybe Passport
validate pass =
    validateBYR pass
    >>= validateIYR
    >>= validateEYR
    >>= validateHGT
    >>= validateHCL
    >>= validateECL
    >>= validatePID

-- four digits; at least 1920 and at most 2002.
validateBYR :: Passport -> Maybe Passport
validateBYR pass = do
  field <- Map.lookup "byr" pass
  year <- Util.safeRead field
  if year >= 1920 && year <= 2002
  then return pass
  else fail "Bad Birth Year!"


-- four digits; at least 2010 and at most 2020.
validateIYR :: Passport -> Maybe Passport
validateIYR pass = do
  field <- Map.lookup "iyr" pass
  year <- Util.safeRead field
  if year >= 2010 && year <= 2020
  then return pass
  else fail "Bad Issue Year!"


-- four digits; at least 2020 and at most 2030.
validateEYR :: Passport -> Maybe Passport
validateEYR pass = do
  field <- Map.lookup "eyr" pass
  year <- Util.safeRead field
  if year >= 2020 && year <= 2030
  then return pass
  else fail "Bad Expiration Year!"

-- hgt (Height) - a number followed by either cm or in:
--     If cm, the number must be at least 150 and at most 193.
--     If in, the number must be at least 59 and at most 76.
validateHGT :: Passport -> Maybe Passport
validateHGT pass = do
  field <- Map.lookup "hgt" pass
  let (nrString, unit) = span Char.isNumber field
  height <- Util.safeRead nrString
  if (unit == "cm" && height >= 150 && height <= 193) ||
     (unit == "in" && height >= 59 && height <= 76)
  then return pass
  else fail "Bad Height!"

-- hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
validateHCL :: Passport -> Maybe Passport
validateHCL pass = do
  field <- Map.lookup "hcl" pass
  firstChar <- Util.safeHead field
  rest <- Util.safeTail field
  if firstChar == '#' && all (\c -> elem c "1234567890abcdef") rest && length rest == 6
  then return pass
  else fail "Bad Hair Color!"

-- ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
validateECL :: Passport -> Maybe Passport
validateECL pass = do
  field <- Map.lookup "ecl" pass
  if elem field ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
  then return pass
  else fail "Bad Eye Color!"

-- pid (Passport ID) - a nine-digit number, including leading zeroes.
validatePID :: Passport -> Maybe Passport
validatePID pass = do
  field <- Map.lookup "pid" pass
  if all Char.isNumber field && length field == 9
  then return pass
  else fail "Bad Password ID!"
