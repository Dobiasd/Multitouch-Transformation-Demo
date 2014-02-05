module Common where

{-| Forward function application. -}
(-:) : a -> (a -> b) -> b
x -: f = f x
infixl 0 -:

{-| Apply function to every element in 2-tuple. -}
map2t : (a -> b) -> (a, a) -> (b, b)
map2t f (a, b) = (f a, f b)

{-| Apply function to every element in 3-tuple. -}
map3t : (a -> b) -> (a, a, a) -> (b, b, b)
map3t f (a, b, c) = (f a, f b, f c)

{-| Apply function to every element in 4-tuple. -}
map4t : (a -> b) -> (a, a, a, a) -> (b, b, b, b)
map4t f (a, b, c, d) = (f a, f b, f c, f d)

{-| Bring value into [minX, maxX]. -}
clamp : Ord a => a -> a -> a -> a
clamp minX maxX x = x `max` minX `min` maxX

{-| [[1,4],[2,5],[3,6]] -> [1,2,3,4,5,6] -}
interleave : [[a]] -> [a]
interleave = concat . transpose

{-| 2 [1,2,3,4,5,6] -> [[1,4],[2,5],[3,6]] -}
deinterleave : Int -> [a] -> [[a]]
deinterleave step = transpose . chunksOf step

{-| Are all Elements in the list equal? -}
allTheSame : (Eq a) => [a] -> Bool
allTheSame [] = True
allTheSame (x:xs) = all (== x) xs

{-| 3 [1,2,3,4,5,6,7,8] -> [1,4,7] -}
every : Int -> [a] -> [a]
every _ [] = []
every n xs@(x:_) = x : every n (drop n xs)