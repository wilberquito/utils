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
>>> :t null
null :: forall (t :: * -> *) a. Foldable t => t a -> Bool

>>> take' 5 [1..10]
[1,2,3,4,5]

>>> take' 5 [1..3]
[1,2,3]

>>> take' (-1) [1..3]
[]

>>> take' 1 "hola"
"h"
-}

take' :: Integral a => a -> [b] -> [b]
take' n xs
    | n <= 0 = []
    | null xs = []
    | otherwise = head xs : take' (n-1) (tail xs)


{-
>>> reverse' [1,2,3]
[3,2,1]

reverse [1, 2, 3]
reverse [2, 3] ++ [1]
reverse [3] ++ [2] ++ [1]
reverse [] ++ [3] ++ [2] ++ [1]
[] ++ [3] ++ [2] ++ [1]
-}
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


