module Exercise01.ListFunctions where

import Data.List
import Test.QuickCheck

head' [] = undefined
head' (x:xs) = x

tail' [] = undefined
tail' (x:xs) = xs

init' [] = undefined
init' [x] = []
init' (x:xs) = x:(init xs)

last' [] = undefined
last' [x] = x
last' (x:xs) = last' xs

length' [] = 0
length' (x:xs) = 1 + length' xs

reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x] -- there is NO nicer way

(+++) [] [] = []
(+++) [] ys = ys
(+++) (x:xs) ys = x:((+++) xs ys)

iterate' f x = fx:(iterate' f fx)
               where fx = f x

map' f [] = []
map' f (x:xs) = (f x):(map' f xs)

filter' predicate [] = []
filter' predicate (x:xs) = if predicate x then x:fxs else fxs
                           where fxs = filter' predicate xs 
intersperse' el [] = []
intersperse' el [x] = [x]
intersperse' el (x:xs) = [x, el] ++ intersperse' el xs  

concat' [] = []
concat' (l:ls) = l +++ concat' ls

zipWith' f xs [] = []
zipWith' f [] ys = []
zipWith' f (x:xs) (y:ys) = (f x y):(zipWith' f xs ys)

repeat' x = x:(repeat' x) 

and' [] = undefined
and' [x] = x
and' (x:xs) | x = and' xs
            | otherwise = False

takeWhile' predicate [] = []
takeWhile' predicate (x:xs) | predicate x = x:(takeWhile' predicate xs)
                            | otherwise = []

dropWhile' predicate [] = []
dropWhile' predicate (x:xs) | predicate x = dropWhile' predicate xs
                            | otherwise = x:xs 

maximum' [] = undefined
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- tests

prop_head (NonEmpty list) = head list == head' list
prop_tail (NonEmpty list) = tail list == tail' list
prop_init (NonEmpty list) = init list == init' list
prop_last (NonEmpty list) = last list == last' list
prop_length list = length list == length' list
prop_reverse list = reverse list == reverse' list
prop_plus list1 list2 = list1 ++ list2 == list1 +++ list2
prop_iterate f x depth = take depth (iterate f x) == take depth (iterate' f x) --
prop_map f list = map f list == map' f list --
prop_filter p list = filter p list == filter' p list --
prop_intersperse el list = intersperse el list == intersperse' el list
prop_concat llist = concat llist == concat' llist
prop_zipWith f list1 list2 = zipWith f list1 list2 == zipWith' f list1 list2 --
prop_repeat x depth = take depth (repeat x) == take depth (repeat' x)
prop_and (NonEmpty list) = and list == and' list
prop_takeWhile p list = takeWhile p list == takeWhile' p list --
prop_dropWhile p list = dropWhile p list == dropWhile' p list --
prop_maximum (NonEmpty list) = maximum list == maximum' list

