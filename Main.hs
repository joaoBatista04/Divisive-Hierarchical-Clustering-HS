import System.IO (hFlush, stdout)
import Utils (readCSV, checkK, buildLinks, cutLinks, buildGroups, printGroups, saveGroups)
import Point (Point)

main :: IO ()

main = do
    -- Getting informations from the user (path of input and output files and the parameter K - groups amount)
    putStr "Forneça o nome do arquivo de entrada: "
    hFlush stdout
    fpInput <- getLine

    putStr "Forneça o nome do arquivo de saída: "
    hFlush stdout
    fpOutput <- getLine

    putStr "Forneça o número de grupos (K): "
    hFlush stdout
    kStr <- getLine
    let k = read kStr :: Int

    -- Reading point informations (coordinates and dimensions) from CSV file
    points <- readCSV fpInput

    checkK k points
    
    putStrLn "Agrupamentos:"
    hFlush stdout

    printGroups
        (buildGroups points
            (buildLinks points)
            (cutLinks (buildLinks points) k)
        )
        1

    saveGroups fpOutput (buildGroups points
            (buildLinks points)
            (cutLinks (buildLinks points) k)
        )