module Exercise02.Undup where

-- right to left removal
undup [] = []
undup (x:xs) | x `in_list` xs = undup xs
             | otherwise = x:(undup xs)
-- left to right removal
undup' [] = []
undup' list | r `in_list` l = undup' l
            | otherwise = (undup' l) ++ [r]
            where (l, r) = (init list, last list)

-- equivalent of 'elem'
in_list x [] = False
in_list x (y:ys) | (x == y) = True
                 | otherwise = in_list x ys  


-- check if undup' == Data.List.nub
-- TODO
