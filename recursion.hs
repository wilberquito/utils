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



{-
>>> maximum'' [1,2,3,4,5,6,10,1]
10
>>> maximum'' [1]
1
-}
maximum'' :: Ord a => [a] -> a
maximum'' [] = error "wtf"
maximum'' [x] = x
maximum'' ls = foldl1 (\bef now -> if now > bef then now else bef) ls

{-
>>> reverse'' [1,2,3,4]
[4,3,2,1]
-}
reverse'' :: [a] -> [a]
reverse'' ls = foldl (\bef now -> now : bef) [] ls

{-
>>> product' [1,2,3,4]
24
-}
product' :: (Num a) => [a] -> a
product' = foldl (*) 1

{-
>>> filter' [1,1,1,3,4,2,2,5,8,5] 5
[5,5]
-}
filter' :: Eq a => [a] -> a -> [a]
filter' ls el = foldl (\x y -> if y == el then y:x else x) [] ls


{-
>>> head' [1,2,3]
1
>>> head' []
there is no head in an empty list!
-}
head' :: [a] -> a
head' [] = error "there is no head in an empty list!"
head' ls = foldl1 (\x _ -> x) ls 


{-
>>> last' [1,2,3]
3
>>> last' []
there is no last in an empty list!
-}
last' :: [a] -> a
last' [] = error "there is no last in an empty list!"
last' ls = foldr1 (\_ x -> x) ls 
