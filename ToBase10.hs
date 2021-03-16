
i_toBase10 :: [Int] -> Int -> Int
i_toBase10 ls b
    | ls == [] = 0
    | otherwise = 
        do
            let exp = (length ls) - 1
                (x:xs) = ls
            x*(b^exp) + i_toBase10 xs b               
                    

-- 1. list ordered from higher to lower position
-- 2. base
toBase10 :: [Int] -> Int -> Int
toBase10 ls b
    | ls == [] = error "unexepected empty list"
    | b < 0 = error "unexpected negative base"
    | otherwise = i_toBase10 ls b