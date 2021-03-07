import System.Environment
import System.IO
import Data.Maybe


-- 1. posible a comprar si es inverso
-- 2. numero al que buscamos inverso
-- 3. rango modular
esInversoModular :: Int -> Int -> Int -> Bool
esInversoModular n p m = mod (n*p) m == 1


inversoModular :: [Int] -> Int -> Int -> Maybe Int
inversoModular [] _ _ = Nothing
inversoModular l@(h:ls) n m = if esInversoModular h n m
                            then 
                                Just h
                            else
                                inversoModular ls n m



main = do

    args <- getArgs

    if length args /= 2
    then
        error "\n[0] Número al que se busca inverso\n[1] Rango modular"
    else do
        let n = read (args !! 0) :: Int
            m = read (args !! 1) :: Int
            inverso = inversoModular ([1..m]) n m

        if isNothing inverso
        then 
            putStrLn $ "El número " ++ (show n) ++ " NO tiene inverso modular " ++ (show m) ++ "\n"       
        else 
            putStrLn $ "El inverso modular " ++ (show m) ++ " de " ++ (show n) ++ " es {{" ++ (show $ fromJust inverso) ++ "}}\n"
