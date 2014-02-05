module Matrix ( Matrix
              , identity
              , translate
              , scaleProportional
              , scale
              , rotateOrigin
              , rotatePoint
              , twoPointTransformation
              , threePointTransformation
              , fourPointTransformation
              , transform
              , invert
              , concat ) where

import Data.Monoid

import Vector2D
import Affine
import Perspective

type Elem = Double

data Matrix = Affine MatrixAffine | Perspective MatrixPerspective

instance Monoid Matrix where
    mempty = Affine Affine.identity
    a `mappend` b = a `concat` b

{-| Identity  -}
identity : Matrix
identity = Affine Affine.identity

{-| Translation  -}
translate : Vector -> Matrix
translate = Affine . Affine.translate

{-| Scale  -}
scaleProportional : Elem -> Matrix
scaleProportional = Affine . Affine.scaleProportional

{-| Scale  -}
scale : Elem -> Elem -> Matrix
scale sx sy = Affine $ Affine.scale sx sy

{-| Rotation around (0,0) -}
rotateOrigin : Elem -> Matrix
rotateOrigin = Affine . Affine.rotateOrigin

{-| Calculates an affine matrix of 2D rotation around a given center. -}
rotatePoint : Point -> Elem -> Matrix
rotatePoint point angle = Affine $ Affine.rotatePoint point angle

{-| Calculate a two finger touch transformation. -}
twoPointTransformation : (Vector, Vector) -> (Vector, Vector) -> Matrix
twoPointTransformation srcs dsts =
    Affine $ Affine.twoPointTransformation srcs dsts

{-| transformation bringing the first three onto the last three points. -}
threePointTransformation :
    (Vector, Vector, Vector) ->
    (Vector, Vector, Vector) -> Matrix
threePointTransformation srcs dsts =
    Affine $ Affine.threePointTransformation srcs dsts

{-| transformation bringing the first four onto the last four points. -}
fourPointTransformation :
    (Vector, Vector, Vector, Vector) ->
    (Vector, Vector, Vector, Vector) -> Matrix
fourPointTransformation srcs dsts =
    Perspective $ Perspective.fourPointTransformation srcs dsts

{-| Transform a Point by an affine  -}
transform : Matrix -> Point -> Point
transform (Affine m) = Affine.transform m
transform (Perspective m) = Perspective.transform m

{-| Inverse of a affine transformation  -}
invert : Matrix -> Matrix
invert (Affine m) = Affine $ Affine.invert m
invert (Perspective m) = Perspective $ Perspective.invert m

{-| Concatenates two affine transformation matrixes. -}
concat : Matrix -> Matrix -> Matrix
concat (Affine a) (Affine b) =
    Affine $ a `Affine.concat` b
concat (Perspective a) (Perspective b) =
    Perspective $ a `Perspective.concat` b
concat (Affine a) (Perspective b) =
    Perspective $ (affineToPerspective a) `Perspective.concat` b
concat (Perspective a) (Affine b) =
    Perspective $ a `Perspective.concat` (affineToPerspective b)

{-| Converts an affine transformation matrix to a perspective one. -}
affineToPerspective : MatrixAffine -> MatrixPerspective
affineToPerspective (MatrixAffine ((m11, m12, m13),
                                   (m21, m22, m23))) =
    MatrixPerspective ((m11, m12, m13),
                       (m21, m22, m23),
                       ( 0,   0,  1 ))