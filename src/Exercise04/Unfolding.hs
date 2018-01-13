module Exercise04.Unfolding where

import Data.Maybe

-- Build a list starting from acc value
-- and applying maybeFunc
unfoldr' :: (a -> Maybe (b, a)) -> a -> [b]
unfoldr' maybeFunc acc = case maybeFunc acc of
    Nothing -> []
    Just (a, b) -> a : unfoldr' maybeFunc b

-- Define map & iterate using unfoldr

{-- TODO
map' :: (a -> b) -> [a] -> [b]
map' func list = undefined
--}

iterate'' :: (a -> a) -> a -> [a]
iterate'' func current = unfoldr' (\x -> Just (x, func x)) current