module Vector2D  where

{-| Two dimensional vector functions.
-}

type alias Elem = Float

type alias Vector = (Elem, Elem)
type alias Point = Vector

{-| Euclidian distance to the origin. -}
vLength : Vector -> Elem
vLength (x, y) = sqrt <| x^2 + y^2

{-| Distance between two Points. -}
dist : Point -> Point -> Elem
dist a b = vLength <| a `sub` b

{-| Reverse vector -}
invert : Vector -> Vector
invert (x, y) = (-x, -y)

{-| Vector concatenation -}
add : Vector -> Vector -> Vector
(ax, ay) `add` (bx, by) = ((ax + bx), (ay + by))

{-| Difference Vector -}
sub : Vector -> Vector -> Vector
a `sub` b = a `add` (invert b)

{-| Scale length of a Vector -}
mult : Vector -> Elem -> Vector
(x, y) `mult` s = ((s * x), (s * y))

{-| Angle in the x-y-plane. -}
angle2D : Vector -> Elem
angle2D (x, y) = atan2 x y

{-| Return normalized vector with given angle in the x-y-plane. -}
fromAngle : Elem -> Vector
fromAngle angle = ((sin angle), (cos angle))

{-| Scale vector to length 1. -}
normalize : Vector -> Vector
normalize v = v `mult` (1 / vLength v)