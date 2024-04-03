import System.Win32 (COORD(xPos))
main :: IO()
main = do

--- TEST AREA ---
    -- (1.1) myMergeSort
        --print(myMergeSort [5,3,4,2,1])
    -- (1.2) lucky strike
    print(luckyCheck [5,3,4,2,1])
    print(luckyCheck [1,2,3,4,5])


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

sort :: Ord a => [a] -> [a] -> [a]
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

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)


-- 2. Alberi & funzionali sugli alberi