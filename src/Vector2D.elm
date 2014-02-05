module Vector2D ( Vector(..)
                , Point
                , toPair
                , length
                , dist
                , invert
                , add
                , sub
                , mult
                , angle2D
                , fromAngle
                , normalize ) where

type Elem = Double

newtype Vector = Vector (Elem, Elem)
type Point = Vector

{-| Extract dataEuclidian distance to the origin. -}
toPair : Vector -> (Elem, Elem)
toPair (Vector (x, y)) = (x, y)

{-| Euclidian distance to the origin. -}
length : Vector -> Elem
length (Vector (x, y)) = sqrt $ x**2 + y**2

{-| Distance between two Points. -}
dist : Point -> Point -> Elem
dist a b = length $ a `sub` b

{-| Reverse vector -}
invert : Vector -> Vector
invert (Vector (x, y)) = Vector (-x, -y)

{-| Vector concatenation -}
add : Vector -> Vector -> Vector
(Vector (ax, ay)) `add` (Vector (bx, by)) = Vector ((ax + bx), (ay + by))

{-| Difference Vector -}
sub : Vector -> Vector -> Vector
a `sub` b = a `add` (invert b)

{-| Scale length of a Vector -}
mult : Vector -> Elem -> Vector
(Vector (x, y)) `mult` s = Vector ((s * x), (s * y))

{-| Angle in the x-y-plane. -}
angle2D : Vector -> Elem
angle2D (Vector (x, y)) = atan2 x y

{-| Return normalized vector with given angle in the x-y-plane. -}
fromAngle : Elem -> Vector
fromAngle angle = Vector ((sin angle), (cos angle))

{-| Scale vector to length 1. -}
normalize : Vector -> Vector
normalize v = v `mult` (1 / length v)