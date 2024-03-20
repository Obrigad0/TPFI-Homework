main :: IO()
main = do

--- TEST AREA ---

    -- (1.1) myTakeWhile & myDropWhile
          -- print(myTakeWhile (<3) [1,2,5,7,2,1])
          -- print(myDropWhile (<3) [1,2,5,7,2,1])

    -- (1.2) myRemoveDupsOrd
          -- print(myRemoveDupsOrd  [1,2,5,7,7,8,9,9,9,9,10,11,11,12,45,45,45])
    
    -- (1.3) myRemoveDups
          -- print(myRemoveDups [5,2,1,2,5,7,2,1,2,7])

    -- (2.1) myZipWith
          -- print(myZipWith (+) [1,34,5,7,2,1] [1,2,4,7,2,5])

    -- (2.2) zipWith with Zip
          -- print(withZip (+) [1,34,5,7,2,1] [1,2,4,7,2,5])

    -- (2.2) myMap foldl & foldr
          -- print(myMapFoldr (^2) [1,23,4,66,7])
          print(myMapFoldl (^2) [1,23,4,66,7])

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

-- myRemoveDups ::  [a] -> [a]
-- myRemoveDups [x] = [x]


-- 2. Interdefinibilita' di Funzionali

  -- (2.1) myZipWith

zapp :: [a -> b] -> [a] -> [b]
zapp (f:fs)(x:xs)= f x : zapp fs xs
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

-- myMapFoldl f xs = foldl ((++) . (:[]) . f) [] xs

  -- (2.4) Il limite di map

  {- 
  
    La funzione Map e' limitata a operazioni che operano individualmente su ciascun elemento della lista,
    preservando sempre la struttura stessa della lista. Mentre Foldr e Foldl sono operazioni piu' "libere",
    che possono trasformare una lista in un valore singolo o lasciare intatta la struttura della lista.
  
  -}