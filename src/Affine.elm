module Affine ( MatrixAffine
              , identity
              , translate
              , scaleProportional
              , scale
              , rotateOrigin
              , rotatePoint
              , transform
              , twoPointTransformation
              , threePointTransformation
              , invert
              , concat ) where

import Data.Monoid

import Common
import Vector2D

type Elem = Double

{-| Affine transformation matrix. -}
newtype MatrixAffine = MatrixAffine ((Elem, Elem, Elem),
                                     (Elem, Elem, Elem)) deriving Show

instance Monoid MatrixAffine where
    mempty = identity
    a `mappend` b = a `Affine.concat` b


{-| Identity matrix. -}
identity : MatrixAffine
identity = MatrixAffine ((1, 0, 0),
                         (0, 1, 0))

{-| Translation matrix. -}
translate : Vector -> MatrixAffine
translate (Vector (x, y)) = MatrixAffine ((1, 0, x),
                                          (0, 1, y))

{-| Scale matrix. -}
scaleProportional : Elem -> MatrixAffine
scaleProportional s = scale s s

{-| Scale matrix. -}
scale : Elem -> Elem -> MatrixAffine
scale sx sy = MatrixAffine ((sx,  0, 0),
                            ( 0, sy, 0))

{-| Rotation around (0,0) -}
rotateOrigin : Elem -> MatrixAffine
rotateOrigin a = MatrixAffine ((cos a, -(sin a), 0),
                               (sin a,   cos a , 0))

{-| Calculates an affine matrix of 2D rotation around a given center. -}
rotatePoint : Point -> Elem -> MatrixAffine
rotatePoint (Vector (cx, cy)) angle =
    MatrixAffine (( a, b, (1 - a) * cx - b * cy),
                  (-b, a, b * cx + (1 - a) * cy))
    where
        a = cos angle
        b = sin angle


{-| Transform a Point by an affine matrix. -}
transform : MatrixAffine -> Point -> Point
transform (MatrixAffine ((m11, m12, m13),
                         (m21, m22, m23)))
          (Vector (x, y)) =
    Vector ((m11 * x + m12 * y + m13),
            (m21 * x + m22 * y + m23))


{-| Calculate a two finger touch transformation. -}
twoPointTransformation : (Vector, Vector) -> (Vector, Vector) -> MatrixAffine
twoPointTransformation (s1, s2) (d1, d2) =
    tScale `Affine.concat` tRotate `Affine.concat` tTranslate
    where
        tTranslate = translate (d1 `sub` s1ScaleRow)
        s1ScaleRow = s1 -: (transform tScale) -: (transform tRotate)
        tRotate = rotateOrigin angle
        tScale = scaleProportional scaleFactor
        sDiff = s2 `sub` s1
        dDiff = d2 `sub` d1
        scaleFactor = Vector2D.length dDiff / Vector2D.length sDiff
        angle = angle2D sDiff - angle2D dDiff

-- source: http://stackoverflow.com/questions/1114257/transform-a-triangle-to-another-triangle
{-| Calculate the transformation
    bringing the first three points exactly onto the last three points. -}
threePointTransformation :
    (Vector, Vector, Vector) ->
    (Vector, Vector, Vector) -> MatrixAffine
threePointTransformation
        ((Vector (s1x, s1y)), (Vector (s2x, s2y)), (Vector (s3x, s3y)))
        ((Vector (d1x, d1y)), (Vector (d2x, d2y)), (Vector (d3x, d3y))) =
    MatrixAffine ((a1, a2, a3),
                  (a4, a5, a6))
    where
        a1 = ((d1x-d2x)*(s1y-s3y)-(d1x-d3x)*(s1y-s2y))/
             ((s1x-s2x)*(s1y-s3y)-(s1x-s3x)*(s1y-s2y))
        a2 = ((d1x-d2x)*(s1x-s3x)-(d1x-d3x)*(s1x-s2x))/
             ((s1y-s2y)*(s1x-s3x)-(s1y-s3y)*(s1x-s2x))
        a3 = d1x-a1*s1x-a2*s1y
        a4 = ((d1y-d2y)*(s1y-s3y)-(d1y-d3y)*(s1y-s2y))/
             ((s1x-s2x)*(s1y-s3y)-(s1x-s3x)*(s1y-s2y))
        a5 = ((d1y-d2y)*(s1x-s3x)-(d1y-d3y)*(s1x-s2x))/
             ((s1y-s2y)*(s1x-s3x)-(s1y-s3y)*(s1x-s2x))
        a6 = d1y-a4*s1x-a5*s1y


{-| Determinant of an affine transformation matrix. -}
determinant : MatrixAffine -> Elem
determinant (MatrixAffine ((m11, m12, _),
                           (m21, m22, _))) = (m11 * m22 - m12 * m21)

{-| Inverse of an affine transformation matrix. -}
invert : MatrixAffine -> MatrixAffine
invert m@(MatrixAffine ((m11, m12, m13),
                        (m21, m22, m23))) =
    MatrixAffine ((a11, a12, b1),
                  (a21, a22, b2))
    where
        invDet = 1 / determinant m
        a11 =  m22 * invDet
        a22 =  m11 * invDet
        a12 = -m12 * invDet
        a21 = -m21 * invDet
        b1  = -a11 * m13 - a12 * m23
        b2  = -a21 * m13 - a22 * m23

{-| Concatenates two affine transformation matrixes. -}
concat : MatrixAffine -> MatrixAffine -> MatrixAffine
a `concat` b = threePointTransformation sourcePoints destPoints
    where
        sourcePoints = (Vector (0, 0), Vector (1, 0), Vector (0, 1))
        destPoints = sourcePoints -: map3t (transform a)
                                  -: map3t (transform b)