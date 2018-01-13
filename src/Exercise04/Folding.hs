module Exercise04.Folding where

foldr' _ acc [] = acc
foldr' func acc (x:xs) = foldr' func (func x acc) xs

foldl' _ acc [] = acc
foldl' func acc (x:xs) = foldl' func (func acc x) xs

-- Definitions of or, filter, map, foldl & remdups
-- using foldr

or' :: [Bool] -> Bool
or' list = foldr' (||) False list

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' predicate list = foldr' func [] list
    where func x acc =
            if predicate x
            then acc ++ [x]
            else acc

map'' :: (a -> b) -> [a] -> [b]
map'' mapFunc list = foldr' func [] list
    where func x acc = acc ++ [mapFunc x]

-- foldl defined using foldr:
-- Just switch the accumulator and the element
foldl'' func acc list = foldr' funcRight acc list 
    where funcRight acc x = func x acc 

-- Idea: remove the current element from the
-- accumulator list and then prepend it
remdups :: (Eq a) => [a] -> [a]
remdups list = foldr func list list
    where func x acc = x:(filter (/=x) acc)