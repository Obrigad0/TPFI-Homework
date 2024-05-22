import System.IO
import Data.Char 

main :: IO()
main = do

--- TEST AREA ---

    -- charCount
    print(intToNatBin 90)
    --print(modBin (NatBin [False,False,False,False,True,True,True,True]) (NatBin [False,False,False,False,False,False,True,False]))
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
    
        
-- 2. Nodi Equilibrati con Applicativi e Monadi


-- 3. Monadi/Eccezioni

-- Espansione tipo Maybe
data ErroreAritmetico e =  Ris e | DivisionePerZero | NumNegativo | OverFlow deriving (Show)

 -- type NatBin = [Bool] deriving (Eq, Ord, Show)
data NatBin = NatBin [Bool] deriving (Eq, Ord, Show)


natBinToInt :: NatBin -> Int
natBinToInt (NatBin a) = natBinToIntAux (NatBin (reverse a)) 0

natBinToIntAux :: NatBin -> Int -> Int
natBinToIntAux (NatBin []) _ = 0
natBinToIntAux (NatBin (x:xs)) indx = (2^indx) * bit + natBinToIntAux (NatBin xs) (indx + 1) 
        where bit = if x then 1 else 0

intToNatBin :: Int -> NatBin
intToNatBin 0 = NatBin (replicate 8 False)
intToNatBin n = NatBin (aumentaBit (reverse (intToNatBinAux n)))

intToNatBinAux :: Int -> [Bool]
intToNatBinAux 0 = []
intToNatBinAux n = (n `mod` 2 == 1) : intToNatBinAux (n `div` 2)

aumentaBit :: [Bool] -> [Bool]
aumentaBit bits = replicate (8 - length bits) False ++ bits

sumBin :: NatBin -> NatBin -> ErroreAritmetico NatBin
sumBin z@(NatBin a) y@(NatBin b) 
        | numA > 255 || numB > 255 = OverFlow
        | numA + numB > 255 = OverFlow
        | otherwise = Ris (intToNatBin (numA + numB))
                where numA = natBinToInt z
                      numB = natBinToInt y

subBin :: NatBin -> NatBin -> ErroreAritmetico NatBin
subBin z@(NatBin a) y@(NatBin b) 
        | numA > 255 || numB > 255 = OverFlow
        | numA - numB < 0 = NumNegativo
        | otherwise = Ris (intToNatBin (numA - numB))
                where numA = natBinToInt z
                      numB = natBinToInt y

mulBin :: NatBin -> NatBin -> ErroreAritmetico NatBin
mulBin z@(NatBin a) y@(NatBin b) 
        | numA > 255 || numB > 255 = OverFlow
        | numA * numB > 255 = OverFlow
        | otherwise = Ris (intToNatBin (numA * numB))
                where numA = natBinToInt z
                      numB = natBinToInt y

divBin :: NatBin -> NatBin -> ErroreAritmetico NatBin
divBin z@(NatBin a) y@(NatBin b) 
        | numA > 255 || numB > 255 = OverFlow
        | numB == 0 = DivisionePerZero
        | otherwise = Ris (intToNatBin  (numA `div` numB))
                where numA = natBinToInt z
                      numB = natBinToInt y

modBin :: NatBin -> NatBin -> ErroreAritmetico NatBin
modBin z@(NatBin a) y@(NatBin b) 
        | numA > 255 || numB > 255 = OverFlow
        | numB == 0 = DivisionePerZero
        | otherwise = Ris (intToNatBin  (numA `mod` numB))
                where numA = natBinToInt z
                      numB = natBinToInt y

