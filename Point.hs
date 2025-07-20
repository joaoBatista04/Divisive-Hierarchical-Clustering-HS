module Point (Point(..), euclideanDistance, getID) where

-- Class that represents the informations of data points
-- Each point has an id and a list of coordinates
data Point = Point { id :: Int, coordinates :: [Float] } deriving (Show)

-- Calculating euclidean distance between two points, following the formula:
-- sqrt((x1-y1)^2 + (x2-y2)^2 + ... + (xn-yn)^2)
euclideanDistance :: Point -> Point -> Float
euclideanDistance (Point _ coords1) (Point _ coords2) =
    sqrt $ sumSquares coords1 coords2
  where
    -- Helper function to calculate the sum of squared differences
    sumSquares :: [Float] -> [Float] -> Float
    sumSquares [] [] = 0
    sumSquares (x:xs) (y:ys) = (x - y) ^ 2 + sumSquares xs ys

getID :: Point -> Int
getID (Point i _) = i