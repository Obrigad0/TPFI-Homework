main :: IO()
main = do

--- TEST AREA ---

    print(myTakeWhile (<3) [1,2,5,7,2,1])
    -- print(myDropWhile (<3) [1,2,5,7,2,1])
    -- print(myRemoveDupsOrd  [1,2,5,7,7,8,9,9,9,9,10,11,11,12,45,45,45])
    -- print(myRemoveDups [5,2,1,2,5,7,2,1,2,7])
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
