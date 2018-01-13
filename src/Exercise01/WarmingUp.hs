module Exercise01.WarmingUp where

maxi :: Int -> Int -> Int
maxi a b = if a > b then a else b

mini :: Int -> Int -> Int
mini a b = if a < b then a else b

max3 :: Int -> Int -> Int ->Int
max3 a b c = maxi (maxi a b) c

min3 :: Int -> Int -> Int -> Int
min3 a b c = mini (mini a b) c

med :: Int -> Int -> Int -> Int
med a b c = max3 (mini a b) (mini b c) (mini c a)

-- check basics
prop_maxEq x = maxi x x == x
prop_minEq x = mini x x == x

-- compare to library max & min
prop_realMax x y = maxi x y == max x y
prop_realMin x y = mini x y == min x y
prop_realMax3 x y z = max3 x y z == max (max x y) z

-- check if median is the second largest of three
prop_med x y z = medElement <= maxElement && medElement >= minElement
                 where maxElement = max3 x y z
                       minElement = min3 x y z
                       medElement = med x y z


