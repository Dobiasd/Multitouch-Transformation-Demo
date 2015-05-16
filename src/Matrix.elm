module Matrix where

{-| Matrix functions for affine and perspective transformations.
-}

import Vector2D exposing (Vector, Point, sub, vLength, angle2D)

type alias Elem = Float

{-| Perspective transformation matrix. -}
type alias Matrix = ((Elem, Elem, Elem),
                     (Elem, Elem, Elem),
                     (Elem, Elem, Elem))


{-| Transform a point by a perspective matrix. -}
transform : Matrix -> Point -> Point
transform ((m11, m12, m13),
           (m21, m22, m23),
           (m31, m32, m33))
          (x, y) =
    ((m11 * x + m12 * y + m13) /
     (m31 * x + m32 * y + m33),
     (m21 * x + m22 * y + m23) /
     (m31 * x + m32 * y + m33))


{-| Identity matrix. -}
identityMat : Matrix
identityMat = ((1, 0, 0),
            (0, 1, 0),
            (0, 0, 1))

{-| Translation matrix. -}
translate : Vector -> Matrix
translate (x, y) = ((1, 0, x),
                    (0, 1, y),
                    (0, 0, 1))

{-| Scale matrix. -}
scaleProportional : Elem -> Matrix
scaleProportional s = mScale s s

{-| Scale matrix. -}
mScale : Elem -> Elem -> Matrix
mScale sx sy = ((sx,  0, 0),
               ( 0, sy, 0),
               ( 0,  0, 1))

{-| Rotation around (0,0) -}
rotateOrigin : Elem -> Matrix
rotateOrigin a =  ((cos a, -(sin a), 0),
                   (sin a,   cos a , 0),
                   (  0  ,     0   , 1))

{-| Calculate a one finger touch transformation. -}
onePointTransformation : (Vector, Vector) -> Matrix
onePointTransformation (s, d) = translate (d `sub` s)

{-| Calculate a two finger touch transformation. -}
twoPointTransformation : (Vector, Vector) -> (Vector, Vector) -> Matrix
twoPointTransformation (s1, d1) (s2, d2) =
    let
        tTranslate = translate (d1 `sub` s1ScaleRow)
        s1ScaleRow = s1 |> (transform tScale) |> (transform tRotate)
        tRotate = rotateOrigin angle
        tScale = scaleProportional scaleFactor
        sDiff = s2 `sub` s1
        dDiff = d2 `sub` d1
        scaleFactor = vLength dDiff / vLength sDiff
        angle = angle2D sDiff - angle2D dDiff
    in
        tScale `mConcat` tRotate `mConcat` tTranslate


-- source: http://stackoverflow.com/questions/1114257/transform-a-triangle-to-another-triangle
{-| Calculate the transformation
    bringing the first three points exactly onto the last three points. -}
threePointTransformation :
    (Vector, Vector) ->
    (Vector, Vector) ->
    (Vector, Vector) -> Matrix
threePointTransformation
        ((s1x, s1y), (d1x, d1y))
        ((s2x, s2y), (d2x, d2y))
        ((s3x, s3y), (d3x, d3y)) =
    let
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
    in
        ((a1, a2, a3),
         (a4, a5, a6),
         ( 0,  0,  1))

{-| Calculate the transformation
    bringing the first four points exactly onto the last four points. -}
fourPointTransformation :
    (Vector, Vector) ->
    (Vector, Vector) ->
    (Vector, Vector) ->
    (Vector, Vector) -> Matrix
fourPointTransformation
        (s1, d1)
        (s2, d2)
        (s3, d3)
        (s4, d4) =
    let
        t1 = fourPointTransformationFromUnitCube (s1, s2, s3, s4) |> invert
        t2 = fourPointTransformationFromUnitCube (d1, d2, d3, d4)
    in
        t1 `mConcat` t2

-- https://github.com/GNOME/gimp/blob/master/app/core/gimp-transform-utils.c
-- -> gimp_transform_matrix_perspective
{-| Calculate the transformation
    bringing the corners of the unit square ((0, 0), (1, 0), (0, 1), (1, 1))
    exactly onto the four given points. -}
fourPointTransformationFromUnitCube :
    (Vector, Vector, Vector, Vector) -> Matrix
fourPointTransformationFromUnitCube
        ((s1x, s1y), (s2x, s2y),
         (s3x, s3y), (s4x, s4y)) =
    let
        dx1 = s2x - s4x
        dx2 = s3x - s4x
        dx3 = s1x - s2x + s4x - s3x

        dy1 = s2y - s4y
        dy2 = s3y - s4y
        dy3 = s1y - s2y + s4y - s3y

        det1 = dx3 * dy2 - dy3 * dx2
        det2 = dx1 * dy2 - dy1 * dx2
        m31 = if det2 == 0.0 then 1 else det1 / det2

        det3 = dx1 * dy3 - dy1 * dx3
        m32 = if det2 == 0.0 then 1 else det3 / det2

        m11 = s2x - s1x + m31 * s2x
        m12 = s3x - s1x + m32 * s3x
        m13 = s1x

        m21 = s2y - s1y + m31 * s2y
        m22 = s3y - s1y + m32 * s3y
        m23 = s1y

        m33 = 1
    in
        ((m11, m12, m13),
         (m21, m22, m23),
         (m31, m32, m33))


-- http://en.wikipedia.org/wiki/Determinant#3.C2.A0.C3.97.C2.A03_matrices
{-| Determinant of a perspective transformation matrix. -}
determinant : Matrix -> Elem
determinant  ((a, b, c),
              (d, e, f),
              (g, h, i)) =
    a*e*i + b*f*g + c*d*h - c*e*g - b*d*i - a*f*h


-- https://github.com/GNOME/gimp/blob/master/libgimpmath/gimpmatrix.c
-- -> gimp_matrix3_invert
{-| Inverse of a perspective transformation matrix. -}
invert : Matrix -> Matrix
invert (((m11, m12, m13),
         (m21, m22, m23),
         (m31, m32, m33)) as m) =
    let
        invDet = 1 / determinant m
        a11 =  (m22 * m33 - m23 * m32) * invDet
        a21 = -(m21 * m33 - m23 * m31) * invDet
        a31 =  (m21 * m32 - m22 * m31) * invDet
        a12 = -(m12 * m33 - m13 * m32) * invDet
        a22 =  (m11 * m33 - m13 * m31) * invDet
        a32 = -(m11 * m32 - m12 * m31) * invDet
        a13 =  (m12 * m23 - m13 * m22) * invDet
        a23 = -(m11 * m23 - m13 * m21) * invDet
        a33 =  (m11 * m22 - m12 * m21) * invDet
    in
        ((a11, a12, a13),
         (a21, a22, a23),
         (a31, a32, a33))

{-| Apply function to every element in 4-tuple. -}
map4t : (a -> b) -> (a, a, a, a) -> (b, b, b, b)
map4t f (a, b, c, d) = (f a, f b, f c, f d)

{-| Concatenates two perspective transformation matrixes. -}
mConcat : Matrix -> Matrix -> Matrix
a `mConcat` b =
    let
        sourcePoints =  ((0, 0), (1, 0), (0, 1), (1, 1))
        destPoints = sourcePoints |> map4t (transform a)
                                  |> map4t (transform b)
    in
        fourPointTransformationFromUnitCube destPoints