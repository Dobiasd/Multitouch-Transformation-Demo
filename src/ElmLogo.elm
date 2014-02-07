module ElmLogo where

{-| Generates a the Elm logo according to a given transformation
-}

import Matrix (Matrix, transform, invert, concat, translate, scale)

import Transform2D

{-| Returns the Elm logo as Form
    With an identity transformation it will result
    centered and sized 300x300. -}
elmLogo : Matrix -> Form
elmLogo matrix =
  let
    t1 = translate (-150, -150)
    t2 = scale 1 -1
    t3 = matrix
    ta = (t1 `concat` t2) `concat` t3
    t = map (transform ta)
    p1 = [(266, 32), (156, 32), (266, 142)] |> t |> polygon
           |> filled (rgb 96 181 204)
    p2 = [(148, 32), (36, 32), (89, 85), (201, 85)] |> t |> polygon
           |> filled (rgb 141 215 55)
    p3 = [(33, 37), (33, 260), (144, 149)] |> t |> polygon
           |> filled (rgb 90 99 120)
    p4 = [(149, 153), (37, 265), (261, 265)] |> t |> polygon
           |> filled (rgb 96 181 204)
    p5 = [(267, 152), (210.5, 208.5), (267, 265)] |> t |> polygon
           |> filled (rgb 239 165 0)
    p6 = [(206, 89), (92, 89), (148, 146)] |> t |> polygon
           |> filled (rgb 239 165 0)
    p7 = [(209.5, 92.5), (152.5, 148.5), (208, 204), (265, 148)]
           |> t |> polygon
           |> filled (rgb 138 212 55)
  in
    group [ p1, p2, p3, p4, p5, p6, p7 ]