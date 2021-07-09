{-
>>> fibonacci 4
10
>>> :t fibonacci
fibonacci :: forall a. Integral a => a -> a
-}
fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = n + fibonacci (n-1)


{-
>>> maximum' [1,2,3,9,5,6]
9
-}
maximum' :: Integral a => [a] -> a
maximum' [] = error "no existe máximo de una lista vacia"
maximum' [x] = x
maximum' (x:xs)
    | x >= maxTail = x
    | otherwise  = maxTail
    where maxTail = maximum' xs


{-
>>> replicate' 5 4
[4,4,4,4,4]
-}
replicate' :: Integral a => a -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

{-
>>> :t show
show :: forall a. Show a => a -> String

>>> :t read
read :: forall a. Read a => String -> a
-}
