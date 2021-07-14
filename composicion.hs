import Data.List 
    ( 
    find
    )
import Data.Maybe

{-
    <!-- 
    problema:
        encuentra la suma de todos los cuadrados impares menores a 10.000
    -->

>>> sumUntilCondition
166650

>>> sumUntilCondition'
166650
-}

sumUntilCondition :: Integral a => a
sumUntilCondition =  sum $ takeWhile (< 10000) $ filter odd $ map (\x -> x*x) [1..]

sumUntilCondition' :: Integral a => a
sumUntilCondition' = sum . takeWhile (< 10000) . filter odd . map (\x -> x*x) $ [1..]


{-
    <!-- 
    problema:
        busca una función dada una clave de un mapa
    -->
>>> phoneBook
[("betty","555-2938"),("bonnie","452-2928"),("patsy","493-2928"),("lucille","205-2928"),("wendy","939-8282"),("penny","853-2492")]

>>> findKey "wilber" phoneBook
Nothing
>>> findKey "wendy" phoneBook
Just "939-8282"
>>> findKey' "wilber" phoneBook
Nothing
>>> findKey' "penny" phoneBook
Just "853-2492"

>>> findKey'' "penny" phoneBook
Just "853-2492"
>>> findKey'' "luis" phoneBook
Nothing
-}

phoneBook :: [([Char], [Char])]
phoneBook =
    [("betty","555-2938")
    ,("bonnie","452-2928")
    ,("patsy","493-2928")
    ,("lucille","205-2928")
    ,("wendy","939-8282")
    ,("penny","853-2492")
    ]


findKey :: Eq k => k -> [(k, v)] -> Maybe v
findKey key ls
    | isNothing search = Nothing
    | otherwise =  Just . snd $ fromJust search
    where
        search = find (\x -> fst x == key) ls

-- de forma recursiva
findKey' :: Eq k => k -> [(k, v)] -> Maybe v
findKey' _ [] = Nothing 
findKey' key ((k,v):xs) =   if key == k
                            then 
                                Just v
                            else 
                                findKey' key xs

-- implementación con pliegue
findKey'' :: Eq k => k -> [(k, v)] -> Maybe v
findKey'' k = foldl (\x y -> if fst y == k then Just $ snd y else x) Nothing



