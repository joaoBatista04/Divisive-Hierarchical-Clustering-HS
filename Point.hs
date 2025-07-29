module Point (Point, euclideanDistance) where

type Point = (Int, [Double])

sumSquaredDiffs :: [Double] -> [Double] -> Double
sumSquaredDiffs [] [] = 0
sumSquaredDiffs (x:xs) (y:ys) = ((x - y)^2) + (sumSquaredDiffs xs ys)

euclideanDistance :: Point -> Point -> Double
euclideanDistance (id1, coords1) (id2, coords2) = sqrt (sumSquaredDiffs coords1 coords2)