import qualified Data.Set as Set 
main :: IO()
main = do

--- TEST AREA ---

    -- (1.1) myTakeWhile & myDropWhile
          -- print(myTakeWhile (<3) [1,2,5,7,2,1])
          -- print(myDropWhile (<3) [1,2,5,7,2,1])

    -- (1.2) myRemoveDupsOrd
          -- print(myRemoveDupsOrd  [1,2,5,7,7,8,9,9,9,9,10,11,11,12,45,45,45])
    
    -- (1.3) myRemoveDups
          print(myRemoveDups [5,2,1,2,5,7,12,1,2,7,12,21,23,12])

    -- (2.1) myZipWith
          -- print(myZipWith (+) [1,34,5,7,2,1] [1,2,4,7,2,5])

    -- (2.2) zipWith with Zip
          -- print(withZip (+) [1,34,5,7,2,1] [1,2,4,7,2,5])

    -- (2.3) myMap foldl & foldr
          -- print(myMapFoldr (^2) [1,23,4,66,7])
          -- print(myMapFoldl (^2) [1,23,4,66,7])

    -- (3.1) Prefissi
          -- print(prefissi [1,2,3,4,5])

    -- (3.2) Prefissi
          -- print(segSommaS [1,2,3,4,5] 5)

    -- (3.3) Prefissi
          -- print(sublSommaS [1,2,3,4,5] 6)
--- ---- ---- ---

-- 1. Rimozione di Duplicati

  -- (1.1) myTakeWhile & myDropWhile

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p [] = []
myTakeWhile p (x:xs)  
    | p x = x : myTakeWhile p xs 
    | otherwise = myTakeWhile p []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile p [] = []
myDropWhile p (x:xs) 
    | p x = myDropWhile p xs 
    | otherwise = x : xs 

  
  -- (1.2) myRemoveDupsOrd

myRemoveDupsOrd :: (Ord a) => [a] -> [a]
myRemoveDupsOrd [x] = [x]
myRemoveDupsOrd (x:xs)
    | x == head xs = myRemoveDupsOrd xs
    | otherwise = x : myRemoveDupsOrd xs


  -- (1.3) myRemoveDups 

  -- O(n) per l'iterazione della lista, O(log n) per operazioni su `Set`
  -- Quindi questa funzione puo' anche essere riscritta utilizzando gli alberi binari di ricerca
  

myRemoveDups :: (Ord a) => [a] -> [a]
myRemoveDups xs = funcRemoveDups xs Set.empty

funcRemoveDups :: (Ord a) => [a] -> Set.Set a -> [a]
funcRemoveDups [] _ = []
funcRemoveDups (x:xs) set 
    | Set.member x set = funcRemoveDups xs set
    | otherwise = x : funcRemoveDups xs (Set.insert x set)

-- Nota: Questa funzione potrebbe essere implementata direttamente utilizzando alberi binari di ricerca.

-- 2. Interdefinibilita' di Funzionali

  -- (2.1) myZipWith

zapp :: [a -> b] -> [a] -> [b]
zapp (f:fs)(x:xs) = f x : zapp fs xs
zapp _ _ = []

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs  = zapp (map f xs) 

  -- (2.2) zipWith with Zip

withZip :: (a -> b -> c) -> [a] -> [b] -> [c]
withZip f xs ys =  map (uncurry f) (zip xs ys)

  -- (2.3) myMap foldl & foldr

myMapFoldr :: (a -> b) -> [a] -> [b]
myMapFoldr f  = foldr ((:) . f) [] 

myMapFoldl :: (a -> b) -> [a] -> [b]
myMapFoldl f  = foldl (\acc x -> acc ++ [f x]) [] 

  -- (2.4) Il limite di map

  {- 

    La funzione Map e' limitata a operazioni che operano individualmente su ciascun elemento della lista,
    preservando sempre la struttura stessa della lista. Mentre Foldr e Foldl sono operazioni piu' "libere",
    che possono trasformare una lista in un valore singolo o lasciare intatta la struttra della lista.
    Inoltre Foldr e Foldl usano un accumulatore (acc) che map non utilizza.
  
  -}


-- 3. Segmenti e sottoliste

  -- (3.1) Prefissi

prefissi :: [a] -> [[a]]
prefissi [] = []
prefissi xs = xs : prefissi (init xs)

  -- (3.2) segSommaS

suffissi :: [a] -> [[a]]
suffissi [] = []
suffissi xs@(_:txs) = xs : suffissi txs

segSommaS :: (Num a, Eq a) => [a] -> a -> [[a]] 
segSommaS [] _ = []
segSommaS xs@(_:txs) s = filter (\x -> sum x == s) (prefissi xs) ++ segSommaS txs s

  -- (3.3) sublSommaS

-- Powerset, funzione introdotta nella lezione 5, genera tutte le sottostringhe possibili di un array
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = map (x:) ts ++ ts where
  ts = powerset xs

sublSommaS :: (Num a, Eq a) => [a] -> a -> [[a]] 
sublSommaS xs s = filter (\x -> sum x == s) (powerset xs) 


-- 4. Partizioni

--  //