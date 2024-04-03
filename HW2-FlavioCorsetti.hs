import System.Win32 (COORD(xPos))
import Control.Arrow (ArrowChoice(right))
main :: IO()
main = do

--- TEST AREA ---

    -- (1.1) myMergeSort
        --print(myMergeSort [5,3,4,2,1])
    -- (1.2) lucky strike
        --print(luckyCheck [5,3,4,2,1])
        --print(luckyCheck [1,2,3,4,5])

    -- (2.1) funzionali BT
        --print(mapBT (^23) (Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 Empty Empty)))
        --print(mapBT' (^7) ( Node' (Node' (Leaf 1) (Node' (Leaf 2) (Leaf 3))) (Node' (Leaf 4) (Leaf 5))))
        --print(foldrBT (:) [] (Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 Empty Empty)))
        --print(foldrBT' (:) [] ( Node' (Node' (Leaf 1) (Node' (Leaf 2) (Leaf 3))) (Node' (Leaf 4) (Leaf 5))))
        --print(foldlBT (:) [] (Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 Empty Empty)))
        print(foldlBT' (:) [] ( Node' (Node' (Leaf 1) (Node' (Leaf 2) (Leaf 3))) (Node' (Leaf 4) (Leaf 5))))

--- ---- ---- ---


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

foldrBT :: (a -> b -> b) -> b -> BinTree a -> b  
foldrBT _ acc Empty = acc
foldrBT f acc (Node x left right) = foldrBT f (f x (foldrBT f acc right)) left

foldrBT' :: (a -> b -> b) -> b -> BinTree' a -> b  
foldrBT' f acc (Leaf a) = f a acc
foldrBT' f acc (Node' left right) = foldrBT' f ( foldrBT' f acc right ) left 

foldlBT :: (a -> b -> b) -> b -> BinTree a -> b  
foldlBT _ acc Empty = acc
foldlBT f acc (Node x left right) = foldlBT f (foldlBT f (f x acc) left) right

foldlBT' :: (a -> b -> b) -> b -> BinTree' a -> b  
foldlBT' f acc (Leaf a) = f a acc
foldlBT' f acc (Node' left right) = foldlBT' f ( foldlBT' f acc left ) right 


    -- (2.2) Funzionali BT