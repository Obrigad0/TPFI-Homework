import System.IO

main :: IO()
main = do

--- TEST AREA ---

    charCount

-----------------


-- 1. Input/Output

{-
charCount :: IO()
charCount = do 
        putStrLn "Inserisci un numero: "
        number <- getLine
        let num = read number :: Int
        count <- getString num
        putStrLn ("Numero di stringhe con ogni lettera:" ++ count )

getString :: Int -> IO Int 
getString 0 = return 0
getString n = do
        putStrLn "Inserisci una stringa: "
        string <- getLine
        contaLettere string + getString n-1

contaLettere :: String -> IO Int
contaLettere s
    | conteggio = return  1
    | otherwise = return 0
    where 
        conteggio = all (`elem` s) ['a' .. 'z']
        -}

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