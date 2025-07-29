module Link (Link(..), secondPoint) where

import Point (Point)

--Each link has an origin point, a final point and a distance
data Link = Link {
    a :: Point,
    b :: Point,
    distance :: Double
}

--To compare two links. If links have the same distance, the algorithm will consider the link with smaller first point (id of the first point)
--Defining '==' operation
instance Eq Link where
    link1 == link2 = compare link1 link2 == EQ

--Defining the way how instances of this type will be compared
instance Ord Link where
    compare (Link a1 b1 distance1) (Link a2 b2 distance2)
        | distance1 /= distance2 = compare distance1 distance2
        | otherwise = compare (min a2 b2, max a2 b2) (min a1 b1, max a1 b1)

--Auxiliary function that accesses the second point of a link (b)
secondPoint :: Link -> Point
secondPoint (Link _ p _) = p