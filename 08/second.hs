-- Advent of Code 2020 Day 8 Part 2 by KovaxG
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
  in show $ filter (\(_, _, b) -> b) $ problem code

problem :: Map Int Instr -> [State]
problem code = go code <$> [1 .. Map.size code]
  where
    go :: Map Int Instr -> InstrPointer -> State
    go code ip =
      finalize (Map.update (Just . flipInstr) ip code) (0, 1, False)


flipInstr :: Instr -> Instr
flipInstr (Nop a) = Jmp a
flipInstr (Jmp a) = Nop a
flipInstr other = other

data Instr = Acc Int | Jmp Int | Nop Int deriving (Show)

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
        _ -> Nop val

type InstrPointer = Int
type Accumulator = Int
type Halted = Bool

type State = (Accumulator, InstrPointer, Halted)

run :: Map Int Instr  -> State -> State
run code (acc, ip, _) =
  case Map.lookup ip code of
    Just (Nop _) ->  (acc, ip + 1, False)
    Just (Acc d) -> (acc + d, ip + 1, False)
    Just (Jmp d) -> (acc, ip + d, False)
    Nothing -> (acc, ip, True)

finalize :: Map Int Instr -> State -> State
finalize code s = go s Set.empty
  where
    go :: State -> Set InstrPointer -> State
    go s set =
      let s1@(_, nip, halted) = run code s
      in if elem nip set || halted
         then s1
         else go s1 (Set.insert nip set)
