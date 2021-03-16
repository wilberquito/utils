

-- this method is thought to be called by `residues`
i_residues :: Int -> Int -> [Int]
i_residues n m 
    | n == 0 = []
    | n > 0 = do
                let r = mod n m
                    n' = n `div` m
                r : i_residues n' m 
    | otherwise = error "something wrong happend searching all residues"

residues :: Int -> Int -> [Int]
residues n m
    | n < 0 || m < 0 = error "this method does not support negative numbers"
    | m == 0 =  error "you can not divide by 0!"
    | otherwise = i_residues n m

