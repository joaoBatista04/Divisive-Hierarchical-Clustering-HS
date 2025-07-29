module Utils (readCSV, checkK, buildLinks, cutLinks, buildGroups, printGroups, saveGroups) where

import System.IO (writeFile, appendFile, hFlush, stdout)
import System.Exit (exitFailure)
import Data.List (minimumBy, sortBy, sort)
import Data.Ord (comparing)

import Point (Point, euclideanDistance)
import Link (Link(..), secondPoint)
import UnionFind (UnionFind, initUF, find, union)

--Function to split a CSV line into comma-separated substrings
splitComma :: String -> [String]
splitComma [] = []
splitComma s =
  let (w, rest) = break (== ',') s
  in w : case rest of
           [] -> []
           (_:xs) -> splitComma xs

--Function to read the CSV file and return a list of points
readCSV :: FilePath -> IO [Point]
readCSV fp = do
  contents <- readFile fp
  let linesOfFile = lines contents
  --For each line, create a point with index and the extracted coordinates
  return $ zipWith parseLine [1..] linesOfFile
  where
    --Function to process a line and convert it to a point
    parseLine :: Int -> String -> Point
    parseLine idx line =
      let coords = map read (splitComma line) :: [Double]
      in (idx, coords)

--Function to check if the user has not placed an invalid value to K parameter
checkK :: Int -> [Point] -> IO()
checkK k points =
  --Checking if the user has not placed an incorrect number of groups
  if length points < k
      then do
        putStrLn "O numero de grupos requerido eh maior do que a quantidade de pontos existentes."
        hFlush stdout
        exitFailure
  else if k < 0
      then do
        putStrLn "O numero de grupos deve ser positivo."
        hFlush stdout
        exitFailure
  else return()

--Function that builds the links
buildLinks :: [Point] -> [Link]
buildLinks points = buildLinksAux points (head points) [head points]

--Auxiliary function for recursion in link construction
buildLinksAux :: [Point] -> Point -> [Point] -> [Link] 
buildLinksAux points curr chosen
    --Base case: all points were visited
    | length chosen == length points = []
    | otherwise = 
        minLink : buildLinksAux points nextPoint (chosen ++ [nextPoint])
  where
    --Finding the link with the shortest distance between unchosen points
    minLink = minimumBy (\l1 l2 -> compare (distance l1) (distance l2)) validLinks
    --List of valid links (those not in the 'chosen' set)
    validLinks = [Link curr p (euclideanDistance curr p) | p <- points, fst p `notElem` map fst chosen]
    --The next point to be chosen
    nextPoint = secondPoint minLink

--Function that cuts links, dividing them into subsets based on the K largest distances
cutLinks :: [Link] -> Int -> [Link]
--Indexes start at zero, so it needs to be K-1 cuts
cutLinks links k = take (k-1) (sortBy (flip (comparing distance)) links)

--Groups the points that remain connected after cutting the links
buildGroups :: [Point] -> [Link] -> [Link] -> [[Point]]
buildGroups points allLinks cutLinks =
    groupByRoot (tagWithRoot (applyUnions (initUF (collectIDs points)) (filterKeptLinks allLinks cutLinks)) points)

--Collects point IDs
collectIDs :: [Point] -> [Int]
collectIDs [] = []
collectIDs ((pid, _) : ps) = pid : collectIDs ps

--Applies all unions to maintained links
applyUnions :: UnionFind -> [Link] -> UnionFind
applyUnions uf [] = uf
applyUnions uf (Link p1 p2 _ : ls) = applyUnions (union uf (fst p1) (fst p2)) ls

--Filters links that have been kept
filterKeptLinks :: [Link] -> [Link] -> [Link]
filterKeptLinks [] _ = []
filterKeptLinks (l:ls) cut =
    if isInList l cut
        then filterKeptLinks ls cut
        else l : filterKeptLinks ls cut

--Checks if a link is in a list
isInList :: Link -> [Link] -> Bool
isInList _ [] = False
isInList l (x:xs) = if l == x then True else isInList l xs

--Associates each point with its root
tagWithRoot :: UnionFind -> [Point] -> [(Int, Point)]
tagWithRoot _ [] = []
tagWithRoot uf (p:ps) = (find uf (fst p), p) : tagWithRoot uf ps

--Groups points by root
groupByRoot :: [(Int, Point)] -> [[Point]]
groupByRoot [] = []
groupByRoot ((r, p):xs) = (p : collectSameRoot r xs) : groupByRoot (removeSameRoot r xs)

--Collect points with the same root
collectSameRoot :: Int -> [(Int, Point)] -> [Point]
collectSameRoot _ [] = []
collectSameRoot r ((rAux, p):xs) =
    if r == rAux then p : collectSameRoot r xs
               else collectSameRoot r xs

--Remove points with the same root
removeSameRoot :: Int -> [(Int, Point)] -> [(Int, Point)]
removeSameRoot _ [] = []
removeSameRoot r ((rAux, p):xs) =
    if r == rAux then removeSameRoot r xs
               else (rAux, p) : removeSameRoot r xs

--Prints the groups: each line is a group with the IDs separated by commas and spaces
printGroups :: [[Point]] -> Int -> IO ()
printGroups [] _ = return ()
printGroups (g:gs) n = do
    printGroupIDs g
    printGroups gs (n + 1)

--Prints only the IDs of the points in a group, separated by a comma
printGroupIDs :: [Point] -> IO ()
printGroupIDs [] = putStrLn ""
printGroupIDs [p] = putStrLn (show (fst p))  --The last point doesn't need a comma after it
printGroupIDs (p:ps) = do
    putStr (show (fst p) ++ ", ")
    printGroupIDs ps

--Saves groups in a file
saveGroups :: FilePath -> [[Point]] -> IO ()
saveGroups filename groups = do
    saveGroupsAux filename groups

--Helper function that performs recursion to save groups in the output file
saveGroupsAux :: FilePath -> [[Point]] -> IO ()
saveGroupsAux _ [] = return ()
saveGroupsAux filename (g:gs) = do
    appendFile filename (groupIDsLine g ++ "\n")
    saveGroupsAux filename gs

--Same idea as the printGroupsIDs function
groupIDsLine :: [Point] -> String
groupIDsLine [] = ""
groupIDsLine [p] = show (fst p)
groupIDsLine (p:ps) = show (fst p) ++ ", " ++ groupIDsLine ps