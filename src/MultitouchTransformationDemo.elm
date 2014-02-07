module MultitouchTransformationDemo where

{-| Small Demonstration of calculating and applying
    different transformation types by user input (1, 2, 3 and 4 fingers)
-}

import Touch
import Window
import ElmLogo (elmLogo)
import Matrix(Matrix, identity, concat, invert)
import Matrix
import Vector2D

type State = { acc:Matrix
             , curr:Matrix
             , size:(Int,Int)
             , touches:[Touch.Touch]
             , pairs:[(Vector2D.Vector, Vector2D.Vector)] }

type Input = { newSize:(Int,Int), newTouches:[Touch.Touch] }

input : Signal Input
input = (Input <~ Window.dimensions ~ Touch.touches)

defaultState : State
defaultState = { acc = identity
               , curr = identity
               , size = (1,1)
               , touches = []
               , pairs = [] }

state : Signal State
state = foldp stepState defaultState input


subPairs ((a, b), (c, d)) ((e, f), (g, h)) = ((a-e, b-f), (c-g, d-h))

stepState : Input -> State -> State
stepState {newSize, newTouches}
    ({acc, curr, size, touches, pairs} as state) =
    let
        (w, h) = size
        wF = toFloat w
        hF = toFloat h
        pairs = makePairs wF hF touches
        touchChange = length newTouches /= length touches

        t = makeTransformation pairs

        (acc', curr', pairs') =
            if touchChange
            then (acc `concat` t, identity, repeat (length touches) ((0,0),(0,0)))
            else (acc, t, pairs)

    in
        { state | size <- newSize
                , touches <- newTouches
                , acc <- acc'
                , curr <- curr'
                , pairs <- pairs' }

main = lift scene state

scene ({acc, curr, size, pairs} as state) =
  let
      (w, h) = size
      wF = toFloat w
      hF = toFloat h
      t = acc `concat` curr
      lines = map (makeLine wF hF) pairs
      logo = collage w h [elmLogo t]
  in
      layers [logo, message, collage w h lines]

touchToPointPair : Touch.Touch -> (Vector2D.Vector, Vector2D.Vector)
touchToPointPair {x, y, x0, y0} = (((toFloat x0), (toFloat y0)),
                                   ((toFloat x ), (toFloat y )))

touchPosToScreenPos : Float -> Float -> Vector2D.Vector -> Vector2D.Vector
touchPosToScreenPos w h (x, y) = (x - w/2, h/2 - y)

makeLine w h pair =
    [fst pair, snd pair] |> map (touchPosToScreenPos w h)
                            |> traced (solid red)

makePairs w h touches =
    let
        map2t f (a, b) = (f a, f b)
        apairs = touches |> map touchToPointPair
                         |> map (map2t (touchPosToScreenPos w h))
    in
        [] ++ apairs

makeTransformation pairs =
    let
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