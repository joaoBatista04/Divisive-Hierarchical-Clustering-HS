import Utils (readCSV, checkK, buildLinks, cutLinks, buildGroups, printGroups, saveGroups)
import Point (Point)

main :: IO ()

main = do
    -- Getting informations from the user (path of input and output files and the parameter K - groups amount)
    putStr "Forneça o nome do arquivo de entrada: "
    fpInput <- getLine

    putStr "Forneça o nome do arquivo de saída: "
    fpOutput <- getLine

    putStr "Forneça o número de grupos (K): "
    kStr <- getLine
    let k = read kStr :: Int

    -- Reading point informations (coordinates and dimensions) from CSV file
    points <- readCSV fpInput

    checkK k points
    
    putStrLn "Agrupamentos:"

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