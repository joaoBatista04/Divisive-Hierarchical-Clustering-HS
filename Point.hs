module Point (Point, euclideanDistance) where

type Point = (Int, [Float])

sumSquaredDiffs :: [Float] -> [Float] -> Float
sumSquaredDiffs [] [] = 0
sumSquaredDiffs (x:xs) (y:ys) = (x - y)^2 + sumSquaredDiffs xs ys

euclideanDistance :: Point -> Point -> Float
euclideanDistance (id1, coords1) (id2, coords2) = sqrt $ sumSquaredDiffs coords1 coords2