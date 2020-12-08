-- Advent of Code 2020 Day 8 Part 1 by KovaxG
-- https://adventofcode.com/2020/day/8

import qualified Util
import           Text.Parsec ((<|>))
import qualified Text.Parsec as Parsec
import qualified Data.Maybe as Maybe
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = Util.runProcess $ \input ->
  let code = Map.fromList $ zip [1..] $ Maybe.catMaybes $ map readInstr $ lines input
  in show $ problem code (0, 1)

data Instr = Acc Int | Jmp Int | Nop deriving (Show)

readInstr :: String -> Maybe Instr
readInstr = Util.eitherToMaybe . Parsec.parse instr ""
  where
    instr = do
      ins <- Parsec.many1 Parsec.letter
      Parsec.spaces
      offset <- Parsec.many1 (Parsec.digit <|> Parsec.char '+' <|> Parsec.char '-')
      let val = read $ filter (/='+') offset
      return $ case ins of
        "acc" -> Acc val
        "jmp" -> Jmp val
        _ -> Nop

type InstrPointer = Int
type Accumulator = Int

run :: Map Int Instr  -> (Accumulator, InstrPointer) -> (Accumulator, InstrPointer)
run code (acc, ip) =
  case Maybe.fromMaybe Nop $ Map.lookup ip code of
    Nop ->  (acc, ip + 1)
    Acc d -> (acc + d, ip + 1)
    Jmp d -> (acc, ip + d)

problem :: Map Int Instr -> (Accumulator, InstrPointer) -> (Accumulator, InstrPointer)
problem code s = go s Set.empty
  where
    go :: (Accumulator, InstrPointer) -> Set InstrPointer -> (Accumulator, InstrPointer)
    go s set =
      let s1@(_, nip) = run code s
      in if elem nip set
         then s1
         else go s1 (Set.insert nip set)