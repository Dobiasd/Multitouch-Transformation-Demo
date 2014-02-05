module Perspective ( MatrixPerspective(..)
                   , identity
                   , transform
                   , fourPointTransformation
                   , invert
                   , concat ) where

import Data.Monoid

import Common
import Vector2D

type Elem = Double

{-| Perspective transformation matrix. -}
newtype MatrixPerspective = MatrixPerspective ((Elem, Elem, Elem),
                                               (Elem, Elem, Elem),
                                               (Elem, Elem, Elem))
                                              deriving Show

instance Monoid MatrixPerspective where
    mempty = identity
    a `mappend` b = a `concat` b


{-| Identity matrix. -}
identity : MatrixPerspective
identity = MatrixPerspective ((1, 0, 0),
                              (0, 1, 0),
                              (0, 0, 1))


{-| Transform a point by a perspective matrix. -}
transform : MatrixPerspective -> Point -> Point
transform (MatrixPerspective ((m11, m12, m13),
                              (m21, m22, m23),
                              (m31, m32, m33)))
          (Vector (x, y)) =
    Vector ((m11 * x + m12 * y + m13) /
            (m31 * x + m32 * y + m33),
            (m21 * x + m22 * y + m23) /
            (m31 * x + m32 * y + m33))


{-| Calculate the transformation
    bringing the first four points exactly onto the last four points. -}
fourPointTransformation :
    (Vector, Vector, Vector, Vector) ->
    (Vector, Vector, Vector, Vector) -> MatrixPerspective
fourPointTransformation sPts dPts = t1 `concat` t2
    where
        t1 = fourPointTransformationFromUnitCube sPts -: invert
        t2 = fourPointTransformationFromUnitCube dPts

-- https://github.com/GNOME/gimp/blob/master/app/core/gimp-transform-utils.c
-- -> gimp_transform_matrix_perspective
{-| Calculate the transformation
    bringing the corners of the unit square ((0, 0), (1, 0), (0, 1), (1, 1))
    exactly onto the four given points. -}
fourPointTransformationFromUnitCube :
    (Vector, Vector, Vector, Vector) -> MatrixPerspective
fourPointTransformationFromUnitCube
        ((Vector (s1x, s1y)), (Vector (s2x, s2y)),
         (Vector (s3x, s3y)), (Vector (s4x, s4y))) =
    MatrixPerspective ((m11, m12, m13),
                       (m21, m22, m23),
                       (m31, m32, m33))
    where
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


-- http://en.wikipedia.org/wiki/Determinant#3.C2.A0.C3.97.C2.A03_matrices
{-| Determinant of a perspective transformation matrix. -}
determinant : MatrixPerspective -> Elem
determinant (MatrixPerspective ((a, b, c),
                                (d, e, f),
                                (g, h, i))) =
    a*e*i + b*f*g + c*d*h - c*e*g - b*d*i - a*f*h


-- https://github.com/GNOME/gimp/blob/master/libgimpmath/gimpmatrix.c
-- -> gimp_matrix3_invert
{-| Inverse of a perspective transformation matrix. -}
invert : MatrixPerspective -> MatrixPerspective
invert m@(MatrixPerspective ((m11, m12, m13),
                             (m21, m22, m23),
                             (m31, m32, m33))) =
    MatrixPerspective ((a11, a12, a13),
                       (a21, a22, a23),
                       (a31, a32, a33))
    where
        invDet = 1 / determinant m
        a11 =   (m22 * m33 - m23 * m32) * invDet
        a21 = - (m21 * m33 - m23 * m31) * invDet
        a31 =   (m21 * m32 - m22 * m31) * invDet
        a12 = - (m12 * m33 - m13 * m32) * invDet
        a22 =   (m11 * m33 - m13 * m31) * invDet
        a32 = - (m11 * m32 - m12 * m31) * invDet
        a13 =   (m12 * m23 - m13 * m22) * invDet
        a23 = - (m11 * m23 - m13 * m21) * invDet
        a33 =   (m11 * m22 - m12 * m21) * invDet


{-| Concatenates two perspective transformation matrixes. -}
concat : MatrixPerspective -> MatrixPerspective -> MatrixPerspective
a `concat` b = fourPointTransformationFromUnitCube destPoints
    where
        sourcePoints = (Vector (0, 0), Vector (1, 0)
                      , Vector (0, 1), Vector (1, 1))
        destPoints = sourcePoints -: map4t (transform a)
                                  -: map4t (transform b)