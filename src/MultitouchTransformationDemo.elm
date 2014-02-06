module MultitouchTransformationDemo where

{-| Small Demonstration of calculating and applying
    different transformation types by user input (1, 2, 3 and 4 fingers)
-}

import Touch
import Window
import ElmLogo (elmLogo)
import Matrix
import Vector2D

main = lift2 scene Window.dimensions Touch.touches

scene (w,h) touches =
  let
      wF = toFloat w
      hF = toFloat h
      lines = map (makeLine wF hF) touches
      transformation = makeTransformation wF hF touches
      logo = collage w h [elmLogo transformation]
  in
      layers [logo, message, collage w h lines]

touchToPointPair : Float -> Float -> Touch.Touch
                         -> (Vector2D.Vector, Vector2D.Vector)
touchToPointPair w h {x, y, x0, y0} = (((toFloat x0), (toFloat y0)),
                                       ((toFloat x ), (toFloat y )))

touchPosToScreenPos : Float -> Float -> Vector2D.Vector -> Vector2D.Vector
touchPosToScreenPos w h (x, y) = (x - w/2, h/2 - y)

makeLine w h touch =
    let pair = touchToPointPair w h touch
    in [fst pair, snd pair] |> map (touchPosToScreenPos w h)
                            |> traced (solid red)

makeTransformation w h touches =
    let
        pairs = map (touchToPointPair w h) touches
        nth n = head . drop n
    in
        case length pairs of
            1 -> Matrix.onePointTransformation   (nth 0 pairs)
            2 -> Matrix.twoPointTransformation   (nth 0 pairs)
                                                 (nth 1 pairs)
            3 -> Matrix.threePointTransformation (nth 0 pairs)
                                                 (nth 1 pairs)
                                                 (nth 2 pairs)
            4 -> Matrix.fourPointTransformation  (nth 0 pairs)
                                                 (nth 1 pairs)
                                                 (nth 2 pairs)
                                                 (nth 3 pairs)
            _ -> Matrix.identity

message = [markdown|
Use 1, 2, 3 or 4 fingers to
move, rotate, scale and distort the elm logo. :-)|]