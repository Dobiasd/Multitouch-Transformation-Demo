module MultitouchTransformationDemo where

{-| Small Demonstration of calculating and applying
    different transformation types by user input (1, 2, 3 and 4 fingers)
-}

import Color exposing (red)
import Debug
import Graphics.Collage exposing (collage, traced, solid)
import Graphics.Element exposing (layers, leftAligned)
import List
import Text
import Touch
import Window
import Signal exposing ((<~),(~))
import Signal
import ElmLogo exposing (elmLogo)
import Matrix exposing (Matrix, identityMat, mConcat, invert)
import Matrix
import Vector2D exposing (Vector, add, sub)

type alias State = { acc:Matrix
                   , curr:Matrix
                   , size:(Int,Int)
                   , touches:List Touch.Touch
                   , offsets:List Vector
                   , pairs :List (Vector,Vector)}

type alias Input = { newSize:(Int,Int), newTouches:List Touch.Touch }

sortTouches : List Touch.Touch -> List Touch.Touch
sortTouches = List.sortBy .t0

input : Signal.Signal Input
input = (Input <~ Window.dimensions ~ (Signal.map sortTouches Touch.touches))

defaultState : State
defaultState = { acc = identityMat
               , curr = identityMat
               , size = (1,1)
               , touches = []
               , offsets = List.repeat 4 (0,0)
               , pairs = [] }

state : Signal.Signal State
state = Signal.foldp stepState defaultState input

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
        numChanged = List.length newTouches /= List.length touches
        newOffsets = List.map pairDiff rawPairs ++ List.repeat 4 (0,0)

        pairs' = if numChanged
                 then List.map2 addPairFst rawPairs newOffsets
                 else List.map2 addPairFst rawPairs offsets

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


main = Signal.map scene state

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
    [fst pair, snd pair] |> List.map (touchPosToScreenPos w h)
                         |> traced (solid red)

makePairs w h touches =
    let
        map2t f (a, b) = (f a, f b)
    in
        touches |> List.map touchToPointPair
                |> List.map (map2t (touchPosToScreenPos w h))

unsafeHead : List a -> a
unsafeHead xs = case xs of
  (x::_) -> x
  _ -> Debug.crash "unsafeHead with empty list"

makeTransformation pairs =
    let
        nth n = List.drop n >> unsafeHead
    in
        case List.length pairs of
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

message = "Use 1, 2, 3 or 4 fingers to\nmove, rotate, scale and distort the elm logo. :-)"
  |> Text.fromString |> leftAligned