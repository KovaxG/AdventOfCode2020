-- Advent of Code 2020 Day 14 Part 1 by KovaxG
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

data State = State
  { memory :: Map Word Word
  , setMask :: Word
  , clearMask :: Word
  } deriving (Show)

initState :: State
initState = State { memory = Map.empty, setMask = 0, clearMask = Bits.complement 0 }

data Line = Mask String | Set Word Word deriving (Show)

readMask :: String -> (Word, Word)
readMask s = (setMask, clearMask)
  where
    setMask = foldl Bits.setBit 0 $ map fst $ filter ((==1) . snd) $ zip [0 ..] $ reverse $ map (\c -> if c == '1' then 1 else 0) s
    clearMask = foldl Bits.clearBit (Bits.complement 0) $ map fst $ filter ((==0) . snd) $ zip [0 ..] $ reverse $ map (\c -> if c == '0' then 0 else 1) s

maskValue :: State -> Word -> Word
maskValue State{setMask, clearMask} v = (v .|. setMask) .&. clearMask

interpret :: State -> Line -> State
interpret s@State{memory} cmd = case cmd of
  Mask mstr -> let (setMask, clearMask) = readMask mstr in s { setMask, clearMask }
  Set addr val -> let nVal = maskValue s val in s { memory = Map.insert addr nVal memory }

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
