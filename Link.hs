module Link (Link(..)) where

import Point (Point(..), getID)

data Link = Link{
    a :: Point,
    b:: Point,
    distance :: Float
} deriving (Show)

instance Eq Link where
    link1 == link2 = compare link1 link2 == EQ

instance Ord Link where
    compare (Link a1 b1 distance1) (Link a2 b2 distance2)
        | distance1 /= distance2  = compare distance1 distance2
        | otherwise = compare (minID a1 b1, maxID a1 b1)
                              (minID a2 b2, maxID a2 b2)
      where
        minID p1 p2 = min (getID p1) (getID p2)
        maxID p1 p2 = max (getID p1) (getID p2)