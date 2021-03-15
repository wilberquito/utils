
add :: Int -> Int -> Int
add a b = (+) a b

normalize :: Int -> Int -> Int
normalize n range 
    | n < 0 = until (\x -> x >= 0) (\x -> x + range) n
    | otherwise = last $ takeWhile (\x -> x >= 0) [n, (n-range)..]

main = do

    args <- getArgs

    if length args /= 2
    then
        error "\n[0] Número al que se busca normalizar\n[1] Rango modular"
    else do
        let n = read (args !! 0) :: Int
            range = read (args !! 1) :: Int

        putStrLn $ "El número " ++ (show n) ++ " módulo " ++ range ++ "\nnormaliado es {{" (show (normalize n range)) ++ "}}"

