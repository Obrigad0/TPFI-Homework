main :: IO()
main = do

--- TEST AREA ---
   -- print(take 100000 insomnia )
   -- print(take 10 tartaglia)
        print ( take 100 numeriFortunati)

-----------------

    -- 1. Insomnia

insomnia :: String
insomnia = unwords [show x ++ " sheep" | x <-[1..] ]

    -- 2. Triangolo di Tartaglia

tartaglia :: [[Int]]
tartaglia = [1] : trtAux [1]
    where
    trtAux xs@(_:txs) = riga : trtAux riga
        where
        riga = 1 : zipWith (+) xs txs ++ [1]

    -- 3. Numeri Fortunati


numeriFortunati :: [Int]
numeriFortunati = 1 : filtro 2 [3,5..]
  where filtro n (x:xs) = x : filtro (n+1) (eliminaNumeri x (x-n) xs)

eliminaNumeri :: Int -> Int -> [Int] -> [Int]
eliminaNumeri n 1 (_:xs) = eliminaNumeri n n xs
eliminaNumeri n m (x:xs) = x : eliminaNumeri n (m-1) xs