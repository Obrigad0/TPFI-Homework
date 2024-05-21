import System.IO
import Data.Char 

main :: IO()
main = do

--- TEST AREA ---

    charCount
    
-----------------

-- 1. Input/Output

charCount :: IO()
charCount = do 
        putStrLn "Inserisci un numero: "
        number <- getLine
        let num = read number :: Int
        count <- getString num [(chr x, 0) | x <- [97..122]]
        putStrLn ("Numero di occorrenze di ogni lettera: \n" ++ show count )

getString :: Int -> [(Char, Int)] -> IO [(Char, Int)]
getString 0 xs = return xs
getString n xs = do
        putStrLn "Inserisci una stringa: "
        string <- getLine
        getString (n-1) (contaLettere (map toLower string) xs)

contaLettere :: String -> [(Char, Int)] -> [(Char, Int)]
contaLettere str xs = [(char, if char `elem` str then occ+1 else occ) | (char, occ) <- xs]
    
        
{-



charCount :: IO ()
charCount = do 
    putStrLn "Inserisci un numero: "
    hFlush stdout
    number <- getLine
    let num = read number :: Int
    count <- getString num
    putStrLn ("Numero di stringhe con ogni lettera: " ++  show count)

getString :: Int -> IO Int 
getString 0 = return 0
getString n = do
    putStrLn "Inserisci una stringa: "
    string <- getLine
    count <- contaLettere string
    rest <- getString (n-1)
    return (count + rest)

contaLettere :: String -> IO Int
contaLettere s
    | conteggio = return 1
    | otherwise = return 0
    where 
        conteggio = all (`elem` s) ['a' .. 'z']
        -}