module Utils (readCSV, checkK) where

import System.IO
import System.Exit (exitFailure)
import Point (Point(..))

-- Function to split a CSV line into comma-separated substrings
splitComma :: String -> [String]
splitComma [] = []
splitComma s =
  let (w, rest) = break (== ',') s
  in w : case rest of
           [] -> []
           (_:xs) -> splitComma xs

-- Function to get the point's coordinates list from a CSV file
readCSV :: FilePath -> IO [Point]
readCSV fp = do
  contents <- readFile fp
  let linesOfFile = lines contents
  -- Extracts the coordinates of each point in each row of the CSV file
  return $ zipWith parseLine [1..] linesOfFile
  where
    -- Parses a single line into a Point with an id and coordinates
    parseLine :: Int -> String -> Point
    parseLine idx line =
      let coords = map read (splitComma line) :: [Float]
      in Point idx coords

-- Function to check if the user has not placed an invalid value to K parameter
checkK :: Int -> [Point] -> IO()
checkK k points =
  -- Checking if the user has not placed an incorrect number of groups
  if length points < k
      then do
        putStrLn "O numero de grupos requerido eh maior do que a quantidade de pontos existentes."
        exitFailure
      else if k < 0
        then do
          putStrLn "O numero de grupos deve ser positivo."
          exitFailure
        else return()