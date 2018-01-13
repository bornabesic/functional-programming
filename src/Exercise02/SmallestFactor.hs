module Exercise02.SmallestFactor where

-- smallest factor of a number n
factor :: Int -> Int
factor 0 = 0
factor 1 = 1
factor n = findFactor n 2

-- iterative part
findFactor :: Int -> Int -> Int
findFactor n k | isInt m = k
               | otherwise = findFactor n (k + 1) 
               where m = (fromIntegral n)/ (fromIntegral k)

-- check if decimal number is whole number
isInt x = x == fromInteger (round x)
