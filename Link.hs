module Link (Link(..), secondPoint) where

import Point (Point)

data Link = Link {
    a :: Point,
    b :: Point,
    distance :: Double
} deriving (Show)

instance Eq Link where
    link1 == link2 = compare link1 link2 == EQ

instance Ord Link where
    compare (Link a1 b1 distance1) (Link a2 b2 distance2)
        | distance1 /= distance2 = compare distance1 distance2
        | otherwise = compare (min a2 b2, max a2 b2) (min a1 b1, max a1 b1)

-- Função que acessa o segundo ponto de um link (b)
secondPoint :: Link -> Point
secondPoint (Link _ p _) = p