{-# LANGUAGE GADTs #-}

module Exercise08.SafeList where

-- Safe lists using GADTs

-- List kinds
data EmptyList
data NonEmptyList

-- t -> data type
-- k -> kind (empty or non-empty)
data SafeList t k where
    Cons :: t -> SafeList t k -> SafeList t NonEmptyList
    Nil :: SafeList t EmptyList

safeHead :: SafeList t NonEmptyList -> t
safeHead (Cons x _) = x

example1 = safeHead (Cons 2 Nil)
{--
Does not compile
example2 = safeHead Nil
--}

-- TODO
-- safeDrop :: Int -> SafeList t NonEmptyList -> SafeList t NonEmptyList
-- safeDrop 0 list = list
-- safeDrop n (Cons element rest) = safeDrop (n - 1) rest 


-- TODO: safeAppend
