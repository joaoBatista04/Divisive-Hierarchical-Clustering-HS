module Utils (readCSV, checkK, buildLinks, cutLinks, buildGroups, printGroups) where

import System.IO
import System.Exit (exitFailure)
import Data.List (minimumBy, sortBy, sort)
import Data.Ord (comparing)

import Point (Point, euclideanDistance)
import Link (Link(..), secondPoint)
import UnionFind (UnionFind, initUF, find, union)

-- Função para dividir uma linha CSV em substrings separadas por vírgula
splitComma :: String -> [String]
splitComma [] = []
splitComma s =
  let (w, rest) = break (== ',') s
  in w : case rest of
           [] -> []
           (_:xs) -> splitComma xs

-- Função para ler o arquivo CSV e retornar uma lista de pontos
readCSV :: FilePath -> IO [Point]
readCSV fp = do
  contents <- readFile fp
  let linesOfFile = lines contents
  -- Para cada linha, cria um ponto com índice e as coordenadas extraídas
  return $ zipWith parseLine [1..] linesOfFile
  where
    -- Função para processar uma linha e convertê-la em um ponto
    parseLine :: Int -> String -> Point
    parseLine idx line =
      let coords = map read (splitComma line) :: [Float]
      in (idx, coords)

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

  -- Função que constrói os links
buildLinks :: [Point] -> [Link]
buildLinks points = buildLinksAux points (head points) [head points]

-- Função auxiliar para recursão
buildLinksAux :: [Point] -> Point -> [Point] -> [Link] 
buildLinksAux points curr chosen
    | length chosen == length points = []  -- Caso base: todos os pontos foram visitados
    | otherwise = 
        minLink : buildLinksAux points nextPoint (chosen ++ [nextPoint])
  where
    -- Encontrando o link com a menor distância entre os pontos não escolhidos
    minLink = minimumBy (\l1 l2 -> compare (distance l1) (distance l2)) validLinks
    -- Lista de links válidos (aqueles que não estão no conjunto 'chosen')
    validLinks = [Link curr p (euclideanDistance curr p) | p <- points, fst p `notElem` map fst chosen]
    -- O próximo ponto a ser escolhido
    nextPoint = secondPoint minLink


-- Função que corta os links e retorna os K maiores
cutLinks :: [Link] -> Int -> [Link]
cutLinks links k = take k (sortBy (flip (comparing distance)) links)

buildGroups :: [Link] -> [Point] -> [[Int]]
buildGroups links points = 
    map sort $ foldr groupByRoot [] points
  where
    -- Inicializa a Union-Find com os IDs dos pontos e faz as uniões
    uf = foldl (\acc link -> union acc (fst (a link)) (fst (secondPoint link))) 
                (initUF [1..length points]) 
                links
    
    -- Função auxiliar para agrupar pontos pelo root
    groupByRoot :: Point -> [[Int]] -> [[Int]]
    groupByRoot (id, _) groups = 
        case lookupGroup root groups of
            Just group -> (id : group) : filter (\g -> g /= group) groups
            Nothing -> [id] : groups
      where root = find uf id

    -- Função para buscar um grupo pelo root
    lookupGroup :: Int -> [[Int]] -> Maybe [Int]
    lookupGroup _ [] = Nothing
    lookupGroup root (g:gs)
      | root == find uf (head g) = Just g
      | otherwise = lookupGroup root gs

-- Imprime os agrupamentos de IDs de pontos
printGroups :: [[Int]] -> IO ()
printGroups groups =
    putStrLn "Agrupamentos:" >>
    mapM_ (putStrLn . joinWithComma . map show) groups
  where
    joinWithComma [] = ""
    joinWithComma [x] = x
    joinWithComma (x:xs) = x ++ ", " ++ joinWithComma xs