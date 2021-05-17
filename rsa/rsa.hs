import Data.List

hola :: String -> String
hola _ = "holaaa"


{-- 
    first parameter says me how many
    raw many letter i will not to represent the plain text
    second one says me in how many real letter I will need to represent a block
--}




public_n:: Int
public_n = foldr (*) 1 private_primes

private_phi :: Int
private_phi = foldr (*) 1 $ map (\x -> x-1) private_primes

private_primes :: [Int]
private_primes = [281, 167]

public_e :: Int
public_e = 39423


isPrime :: Float -> Bool
isPrime p = factors == []
    where 
        factors = take 1 $ [(a,b) | a <- [2..(sqrt p)], b <- [2..(p-1)], a*b == p]


base26BlockLength :: Int -> (Int, Int)
base26BlockLength p = do
                    let ls = takeWhile (\e -> (26^e) < p) [0..]
                        e = ls !! (length ls - 1)
                    
                    if 26^e < (p-1)
                    then
                        (e, e+1)
                    else
                        (e, e)

toBlocks :: String -> Int -> [String]
toBlocks [] _ = []
toBlocks s n = [take n s] ++ toBlocks (drop n s) n


encript :: String -> String
encript plainText = do
                        let blocs = toBlocks plainText plainLen
                            encripted = map (\b -> rsa b) blocs
                        intercalate "" encripted

    where 
        (plainLen, encriptLen) = base26BlockLength public_n

rsa :: String -> String
rsa s = "funned" ++ s