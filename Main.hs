import System.IO (hFlush, stdout)
import Utils (readCSV, checkK, buildLinks, cutLinks, buildGroups, printGroups, saveGroups)
import Point (Point)

main :: IO ()

main = do
    --Getting informations from the user (path of input and output files and the parameter K - groups amount)
    putStr "Forneca o nome do arquivo de entrada: "
    hFlush stdout
    fpInput <- getLine

    putStr "Forneca o nome do arquivo de saida: "
    hFlush stdout
    fpOutput <- getLine

    putStr "Forneca o número de grupos (K): "
    hFlush stdout
    kStr <- getLine
    let k = read kStr :: Int

    --Reading point informations (coordinates and dimensions) from CSV file
    points <- readCSV fpInput

    --Checking if the user has not placed an incorrect number of groups
    checkK k points
    
    --Results
    putStrLn "Agrupamentos:"
    hFlush stdout

    --Builds and cuts links from points and defines groups from the information obtained (let is used to avoid double execution of code later) 
    let links = buildLinks points
    let groups = buildGroups points links (cutLinks links k)

    --Preparing the results for printing on stdout (the number 1 passed as a parameter identifies that the groups must be numbered starting from 1)
    printGroups groups 1

    --Preparing the results for saving in the file
    saveGroups fpOutput groups