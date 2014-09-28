module MultitouchTransformationDemo where

{-| Small Demonstration of calculating and applying
    different transformation types by user input (1, 2, 3 and 4 fingers)
-}

import Touch
import Window
import ElmLogo (elmLogo)
import Matrix (Matrix, identityMat, mConcat, invert)
import Matrix
import Vector2D (Vector, add, sub)

type State = { acc:Matrix
             , curr:Matrix
             , size:(Int,Int)
             , touches:[Touch.Touch]
             , offsets:[Vector]
             , pairs :[(Vector,Vector)]}

type Input = { newSize:(Int,Int), newTouches:[Touch.Touch] }

sortTouches : [Touch.Touch] -> [Touch.Touch]
sortTouches = sortBy .t0

input : Signal Input
input = (Input <~ Window.dimensions ~ (lift sortTouches Touch.touches))

defaultState : State
defaultState = { acc = identityMat
               , curr = identityMat
               , size = (1,1)
               , touches = []
               , offsets = repeat 4 (0,0)
               , pairs = [] }

state : Signal State
state = foldp stepState defaultState input

addPairFst (a, b) c = (a `add` c, b)

pairDiff : (Vector, Vector) -> Vector
pairDiff (a, b) = b `sub` a

stepState : Input -> State -> State
stepState {newSize, newTouches}
    ({acc, curr, size, touches, offsets, pairs} as state) =
    let
        (w, h) = size
        (wF, hF) = (toFloat w, toFloat h)
        rawPairs = makePairs wF hF newTouches
        numChanged = length newTouches /= length touches
        newOffsets = map pairDiff rawPairs ++ repeat 4 (0,0)

        pairs' = if numChanged
                 then zipWith addPairFst rawPairs newOffsets
                 else zipWith addPairFst rawPairs offsets

        offsets' = if numChanged
                   then newOffsets
                   else offsets

        t = makeTransformation pairs'

        (acc', curr') =
            if numChanged
            then ( acc `mConcat` curr, t)
            else (acc, t)

    in
        { state | size <- newSize
                , touches <- newTouches
                , acc <- acc'
                , curr <- curr'
                , offsets <- offsets'
                , pairs <- pairs' }


main = lift scene state

scene ({acc, curr, size, touches, offsets, pairs} as state) =
  let
      (w, h) = size
      (wF, hF) = (toFloat w, toFloat h)
      t = acc `mConcat` curr
      logo = collage w h [elmLogo t]
  in
      layers [logo, message]

touchToPointPair : Touch.Touch -> (Vector, Vector)
touchToPointPair {x, y, x0, y0} = (((toFloat x0), (toFloat y0)),
                                   ((toFloat x ), (toFloat y )))

touchPosToScreenPos : Float -> Float -> Vector -> Vector
touchPosToScreenPos w h (x, y) = (x - w/2, h/2 - y)

makeLine w h pair =
    [fst pair, snd pair] |> map (touchPosToScreenPos w h)
                         |> traced (solid red)

makePairs w h touches =
    let
        map2t f (a, b) = (f a, f b)
    in
        touches |> map touchToPointPair
                |> map (map2t (touchPosToScreenPos w h))

makeTransformation pairs =
    let
        nth n = drop n >> head
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
            _ -> Matrix.identityMat

message = [markdown|
Use 1, 2, 3 or 4 fingers to
move, rotate, scale and distort the elm logo. :-)|]