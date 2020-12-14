-- Advent of Code 2020 Day 14 Part 2 by KovaxG
-- https://adventofcode.com/2020/day/14

{-# LANGUAGE NamedFieldPuns #-}

import qualified Util
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Bits ((.|.), (.&.))
import qualified Data.Bits as Bits
import           Text.Parsec ((<|>))
import qualified Text.Parsec as Parsec
import qualified Data.Maybe as Maybe

type Mask = String

data State = State
  { memory :: Map Word Word
  , mask :: Mask
  } deriving (Show)

initState :: State
initState = State { memory = Map.empty, mask = replicate 36 '0' }

data Line = Mask String | Set Word Word deriving (Show)

maskAddrs :: Word -> Mask -> [Word]
maskAddrs w =
  map listToWord
  . traverse (\(i, m) -> cmb m $ Bits.testBit w i)
  . zip [0..]
  . reverse
  where
    listToWord = foldl (\a (i,e) -> if e == 1 then Bits.setBit a i else a) 0 . zip [0..]
    bti b = if b then 1 else 0
    cmb 'X' _ = [0, 1]
    cmb '0' e = [bti e]
    cmb '1' _ = [1]

interpret :: State -> Line -> State
interpret s@State{ memory, mask } cmd = case cmd of
  Mask mask -> s { mask }
  Set addr val -> s { memory = foldl (\m a -> Map.insert a val m) memory $ maskAddrs addr mask }

parseInstr :: String -> Maybe Line
parseInstr = Util.eitherToMaybe . Parsec.parse line ""
  where
    line = Parsec.try mask  <|> Parsec.try set
    mask = Parsec.string "mask = " >> Parsec.many1 (Parsec.digit <|> Parsec.char 'X') >>= return . Mask
    set = do
      Parsec.string "mem["
      addr <- read <$> Parsec.many1 Parsec.digit
      Parsec.string "] = "
      val <- read <$> Parsec.many1 Parsec.digit
      return $ Set addr val

main :: IO ()
main = Util.runProcess $ show . sum . Map.elems . memory . foldl interpret initState . Maybe.mapMaybe parseInstr . lines
