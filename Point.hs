module Point (Point, euclideanDistance) where

type Point = (Int, [Double])

--Auxiliary function that calculates the sum of the squares of the differences in the coordinates of the points
sumSquaredDiffs :: [Double] -> [Double] -> Double
sumSquaredDiffs [] [] = 0
sumSquaredDiffs (x:xs) (y:ys) = ((x - y)^2) + (sumSquaredDiffs xs ys)

--Calculating euclidean distance between two points, following the formula: sqrt((x1-y1)^2 + (x2-y2)^2 + ... + (xn-yn)^2)
euclideanDistance :: Point -> Point -> Double
euclideanDistance (id1, coords1) (id2, coords2) = sqrt (sumSquaredDiffs coords1 coords2)