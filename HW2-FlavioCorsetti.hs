main :: IO()
main = do

--- TEST AREA ---

    -- (1.1) myMergeSort
        print(myMergeSort [5,3,4,2,1])
    -- (1.2) lucky strike
        --print(luckyCheck [5,3,4,2,1])
        --print(luckyCheck [1,2,3,4,5])

    -- (2.1) funzionali BT
        --print(mapBT (^23) (Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 Empty Empty)))
        --print(mapBT' (^7) ( Node' (Node' (Leaf 1) (Node' (Leaf 2) (Leaf 3))) (Node' (Leaf 4) (Leaf 5))))
        --print(foldrBT (:) (++) [] (Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 Empty Empty)))
        --print(foldrBT' (+) (+) 2 ( Node' (Node' (Leaf 1) (Node' (Leaf 2) (Leaf 3))) (Node' (Leaf 4) (Leaf 5))))
        --print(foldlBT (/) 2 (Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 Empty Empty)))
        --print(foldlBT' (/) 2 ( Node' (Node' (Leaf 1) (Node' (Leaf 2) (Leaf 3))) (Node' (Leaf 4) (Leaf 5))))
        --print(countNodi (Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 Empty Empty)))
        --print(countNodi'  ( Node' (Node' (Leaf 1) (Node' (Leaf 2) (Leaf 3))) (Node' (Leaf 4) (Leaf 5))))

    -- altri test

        --print(log 2.71)
        --print(altezzaAlbero' ( Node' (Node' (Leaf 1) (Node' (Leaf 2) (Node' (Leaf 2) (Leaf 3)))) (Node' (Node' (Leaf 2) (Node' (Leaf 2) (Node' (Leaf 2) (Leaf 3)))) (Leaf 5))))
        --print(altezzaAlbero (Node 1 (Node 2 (Node 4 (Node 4 (Node 4 (Node 4 (Node 4 Empty Empty) Empty) Empty) Empty) (Node 4 Empty (Node 4 Empty Empty))) (Node 5 Empty Empty)) (Node 3 Empty Empty)))
        --print( mis (Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 Empty Empty)))
        --print( mis' ( Node' (Node' (Leaf 1) (Node' (Leaf 2) (Node' (Node' (Leaf 2) (Leaf 3)) (Leaf 3)))) (Node' (Leaf 4) (Leaf 5))))
        --print ( nodiEquilibrati (Node 3 (Node 2 (Node 1 (Node 4 Empty Empty) Empty) (Node 0 Empty Empty)) (Node 2 (Node 1 Empty Empty) Empty)) )--- ---- ---- ---
        --print ( listToABR [8,13,2,5,24,7,15,18] )

    -- facoltativo
    --print (mapT (^2) (R 1 [R 2 [R 3 [R 3 []]], R 3 [R 3 [R 3 [R 3 []]]]]))
    --print (foldrT (:) (++) [] (R 1 [R 2 [R 4 [],R 5 []],R 3 []]))



-- 1. mergeSort “iterativo”

    -- (1.1) myMergeSort

myMergeSort :: (Ord a) => [a] -> [a]
myMergeSort [x] = [x]
myMergeSort xs = interval (map (:[]) xs)

interval :: (Ord a) => [[a]] -> [a]
interval [x] = x
interval xs = interval (merge xs)

merge :: (Ord a) => [[a]] -> [[a]]
merge [] = []
merge [x] = [x]
merge (x:y:xs) = sort x y : merge xs

sort :: (Ord a) => [a] -> [a] -> [a]
sort [] ys = ys
sort xs [] = xs
sort xs@(x:txs) ys@(y:tys)
    | x < y = x : sort txs ys
    | otherwise = y : sort xs tys

    -- (1.2) lucky strike

luckyCheck :: (Ord a) => [a] -> [a]
luckyCheck xs
    | isSorted xs = xs
    | otherwise = myMergeSort xs

isSorted :: (Ord a) => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)


-- 2. Alberi & funzionali sugli alberi

data BinTree a = Node a (BinTree a) (BinTree a) | Empty deriving Show
data BinTree' a = Node' (BinTree' a) (BinTree' a) | Leaf a deriving Show

    -- (2.1) Funzionali BT

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT _ Empty = Empty
mapBT f (Node x left right) = Node (f x) (mapBT f left) (mapBT f right)

mapBT' :: (a -> b) -> BinTree' a -> BinTree' b
mapBT' f (Leaf a) = Leaf (f a)
mapBT' f (Node' left right) = Node' (mapBT' f left) (mapBT' f right)

foldrBT :: (a -> b -> b) -> (b -> b -> b) -> b -> BinTree a -> b
foldrBT _ _ acc Empty = acc
foldrBT f f2 acc (Node x left right) = f x (f2 (foldrBT f f2 acc left) (foldrBT f f2 acc right))

foldrBT' :: (a -> b -> b) -> (b -> b -> b) -> b -> BinTree' a -> b
foldrBT' f _ acc (Leaf a) = f a acc
foldrBT' f f2 acc (Node' left right) = f2 (foldrBT' f f2 acc left) (foldrBT' f f2 acc right)


---
foldlBT :: (a -> b -> a) -> a -> BinTree b -> a
foldlBT _ acc Empty = acc
foldlBT f acc (Node x left right) = foldlBT f (foldlBT f (f acc x) left) right

foldlBT' :: (a -> b -> a) -> a -> BinTree' b -> a
foldlBT' f acc (Leaf a) = f acc a
foldlBT' f acc (Node' left right) = foldlBT' f ( foldlBT' f acc left ) right

---


    -- (2.2) Funzionali BT

        -- (a)

countNodi :: (Num b) => BinTree a -> b
countNodi  = foldrBT (\x -> (+) 1) (+) 0

countNodi' :: (Num b) => BinTree' a -> b
countNodi'  = foldrBT' (\x -> (+) 1) (\x y-> x+y+1 ) 0

        -- (b)

altezzaAlbero :: BinTree a -> Int
altezzaAlbero t = foldrBT (\x -> (+) 1) max 0 t - 1

altezzaAlbero' :: BinTree' a -> Int
altezzaAlbero' t = foldrBT' (\x -> (+) 1) (\x y -> max x y +1) 0 t - 1

        -- (c)

-- costo computazionale delle funzioni O(n log n) nel caso medio e O(n^2) nel caso peggiore 

mis :: BinTree a -> Int
mis Empty = 0
mis (Node x left right) = max (abs (altezzaAlbero left - altezzaAlbero right)) (max (mis left) (mis right))

mis' :: BinTree' a -> Int
mis' (Leaf a) = 0
mis' (Node' left right) = max (abs (altezzaAlbero' left - altezzaAlbero' right)) (max (mis' left) (mis' right))


-- FACOLTATIVO  

data Tree a = R a [Tree a] deriving Show   -- "a" valore , [Tree a] figli

mapT :: (a -> b) -> Tree a -> Tree b
mapT f (R x []) = R (f x) []
mapT f (R x xs) = R (f x) (map (mapT f) xs)

{-
foldrT :: (a -> b -> b) -> (b -> b -> b) -> b -> Tree a -> b
foldrT _ _ acc (R _ []) = acc
foldrT f f2 acc (R x xs) = f x (foldr (f2 . foldrT f f2 acc) acc xs)


foldlT :: (a -> b -> a) -> b -> Tree a -> b
foldlT _  acc (R _ []) = acc
foldlT f  acc (R x xs) = foldl f (f acc x) (map (foldlT f acc) xs)
-}
---  

-- 3. Nodi Equilibrati


nodiEquilibrati :: (Num a, Eq a) => BinTree a -> [a]
nodiEquilibrati t = snd (nodiEquilibratiAux t 0)

nodiEquilibratiAux :: (Num a, Eq a) => BinTree a -> a -> (a,[a]) -- Radicato,[valori nodi equilibrati]
nodiEquilibratiAux Empty _ = (0,[])
nodiEquilibratiAux (Node x left right) cammino
    | fst leftTree + fst rightTree + x == cammino = (fst leftTree + fst rightTree + x, x : nodi)
    | otherwise = (fst leftTree + fst rightTree + x, nodi)
    where
      leftTree = nodiEquilibratiAux left (cammino + x)
      rightTree = nodiEquilibratiAux right (cammino + x)
      nodi = snd leftTree ++ snd rightTree

-- !!ANALISI!!
--
-- La funzione nodiEquilibrati effettua un'analisi completa dell'albero binario, 
-- visitando ogni nodo una sola volta. Questo è reso possibile dall'uso della tecnica del tupling, 
-- che consente di accumulare e trasportare i valori necessari durante la visita dell'albero.
-- La COMPLESSITA' TEMPORTALE della funzione è lineare rispetto al numero di nodi presenti nell'albero, ovvero 
-- O(n), dove n rappresenta il numero totale di nodi. 


-- 4. Alberi Binari di ricerca

listToABR :: (Ord a) => [a] -> BinTree a
listToABR xs = listToABRaUX xs Empty

listToABRaUX :: (Ord a) => [a] -> BinTree a -> BinTree a
listToABRaUX xs t = foldl inserimentoABR t xs

inserimentoABR :: (Ord a) => BinTree a -> a -> BinTree a
inserimentoABR Empty valore = Node valore Empty Empty
inserimentoABR (Node x left right) valore
    | valore < x = Node x (inserimentoABR left valore) right
    | otherwise = Node x left (inserimentoABR right valore)

-- !!ANALISI!!
--
-- il costo della funzione e' O(n log n).
-- listToABRaUX scorre la lista una volta, quindi ha costo O(n)
-- inserimentoABR e' una visita di un albero e costa O(log n)
-- Se la lista e' ordinata, il costo di listToABR diventa O(n^2)



-- 5. Derivazioni di programmi

 -- scanr f e = map (foldr f e) . tails 

{-
    CASO []:

        scanr f e []
        = {def. di scanr}
        map (foldr f e) (tails [])
        = {def. di tails}
        map (foldr f e) [[]]
        = {def. di map}
        [foldr f e []]
        = {def. di foldr}
        [e]


    CASO (x:xs):

        scanr f e (x:xs)
        = {def. di scanr}
        map (foldr f e) (tails (x:xs))
        = {def. di tails}
        map (foldr f e) ((x:xs) : tails xs)
        = {def. di map}
        foldr f e (x:xs) : map (foldr f e) tails xs
        = { applicazione ipotesi iniziale  scanr f e = map (foldr f e) . tails }
        foldr f a (x:xs) : scanr f e xs
        = {trasformazione foldr}
        -- 
-}