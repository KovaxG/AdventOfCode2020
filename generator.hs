import qualified Data.Foldable as Foldable
import qualified System.Directory as Dir

main :: IO ()
main = generateAOCStructure

year :: Int
year = 2020

generateAOCStructure :: IO ()
generateAOCStructure = do
  putStrLn "Generating folders..."
  Foldable.traverse_ mkDayFolder [1 .. 25]
  putStrLn "Done."

mkDayFolder :: Int -> IO ()
mkDayFolder nr = do
  let dirName = show nr
  Dir.createDirectory dirName
  Dir.setCurrentDirectory dirName
  writeFile "first.hs" $ generateTemplate nr 1
  writeFile "input.txt" ""
  writeFile "second.hs" $ generateTemplate nr 2
  Dir.setCurrentDirectory ".."

generateTemplate :: Int -> Int -> String
generateTemplate nr part = unlines
  [ "-- Advent of Code " ++ show year ++  " Day " ++ show nr ++  " Part " ++ show part ++ " by KovaxG"
  , "-- https://adventofcode.com/" ++ show year ++ "/day/" ++ show nr
  , ""
  , "main :: IO ()"
  , "main = readFile \"input.txt\" >>= putStrLn . process"
  , ""
  , "process :: String -> String"
  , "process s = s"
  ]
