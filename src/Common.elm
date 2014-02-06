module Common where

{-| Apply function to every element in 2-tuple. -}
map2t : (a -> b) -> (a, a) -> (b, b)
map2t f (a, b) = (f a, f b)

{-| Apply function to every element in 3-tuple. -}
map3t : (a -> b) -> (a, a, a) -> (b, b, b)
map3t f (a, b, c) = (f a, f b, f c)

{-| Apply function to every element in 4-tuple. -}
map4t : (a -> b) -> (a, a, a, a) -> (b, b, b, b)
map4t f (a, b, c, d) = (f a, f b, f c, f d)