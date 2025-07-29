module Utils (readCSV, checkK, buildLinks, cutLinks, buildGroups, printGroups, saveGroups) where

import System.IO (writeFile, appendFile, hFlush, stdout)
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
      let coords = map read (splitComma line) :: [Double]
      in (idx, coords)

-- Function to check if the user has not placed an invalid value to K parameter
checkK :: Int -> [Point] -> IO()
checkK k points =
  -- Checking if the user has not placed an incorrect number of groups
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
cutLinks links k = take (k-1) (sortBy (flip (comparing distance)) links)

-- Agrupa os pontos que permanecem conectados após cortar os links
buildGroups :: [Point] -> [Link] -> [Link] -> [[Point]]
buildGroups points allLinks cutLinks =
    groupByRoot (tagWithRoot (applyUnions (initUF (collectIDs points)) (filterKeptLinks allLinks cutLinks)) points)

-- Coleta os IDs dos pontos
collectIDs :: [Point] -> [Int]
collectIDs [] = []
collectIDs ((pid, _) : ps) = pid : collectIDs ps

-- Aplica todas as uniões nos links mantidos
applyUnions :: UnionFind -> [Link] -> UnionFind
applyUnions uf [] = uf
applyUnions uf (Link p1 p2 _ : ls) = applyUnions (union uf (fst p1) (fst p2)) ls

-- Filtra os links que foram mantidos
filterKeptLinks :: [Link] -> [Link] -> [Link]
filterKeptLinks [] _ = []
filterKeptLinks (l:ls) cut =
    if isInList l cut
        then filterKeptLinks ls cut
        else l : filterKeptLinks ls cut

-- Verifica se um link está em uma lista
isInList :: Link -> [Link] -> Bool
isInList _ [] = False
isInList l (x:xs) = if l == x then True else isInList l xs

-- Associa cada ponto ao seu root
tagWithRoot :: UnionFind -> [Point] -> [(Int, Point)]
tagWithRoot _ [] = []
tagWithRoot uf (p:ps) = (find uf (fst p), p) : tagWithRoot uf ps

-- Agrupa os pontos por root
groupByRoot :: [(Int, Point)] -> [[Point]]
groupByRoot [] = []
groupByRoot ((r, p):xs) = (p : collectSameRoot r xs) : groupByRoot (removeSameRoot r xs)

-- Coleta os pontos com o mesmo root
collectSameRoot :: Int -> [(Int, Point)] -> [Point]
collectSameRoot _ [] = []
collectSameRoot r ((r', p):xs) =
    if r == r' then p : collectSameRoot r xs
               else collectSameRoot r xs

-- Remove os pontos com o mesmo root
removeSameRoot :: Int -> [(Int, Point)] -> [(Int, Point)]
removeSameRoot _ [] = []
removeSameRoot r ((r', p):xs) =
    if r == r' then removeSameRoot r xs
               else (r', p) : removeSameRoot r xs

-- Imprime os grupos: cada linha é um grupo com os IDs separados por vírgula e espaço
printGroups :: [[Point]] -> Int -> IO ()
printGroups [] _ = return ()
printGroups (g:gs) n = do
    printGroupIDs g
    printGroups gs (n + 1)

-- Imprime apenas os IDs dos pontos de um grupo, separados por ", "
printGroupIDs :: [Point] -> IO ()
printGroupIDs [] = putStrLn ""
printGroupIDs [p] = putStrLn (show (fst p))  -- Último ponto, sem vírgula
printGroupIDs (p:ps) = do
    putStr (show (fst p) ++ ", ")
    printGroupIDs ps

-- Salva os grupos num arquivo com formato idêntico ao printGroups
saveGroups :: FilePath -> [[Point]] -> IO ()
saveGroups filename groups = do
    saveGroupsAux filename groups

saveGroupsAux :: FilePath -> [[Point]] -> IO ()
saveGroupsAux _ [] = return ()
saveGroupsAux filename (g:gs) = do
    appendFile filename (groupIDsLine g ++ "\n")
    saveGroupsAux filename gs

-- Gera a linha de IDs de um grupo separados por ", "
groupIDsLine :: [Point] -> String
groupIDsLine [] = ""
groupIDsLine [p] = show (fst p)
groupIDsLine (p:ps) = show (fst p) ++ ", " ++ groupIDsLine ps