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

instance (Show t) => Show (SafeList t k) where
    show (Cons e r) = (show e) ++ " " ++ (show r)
    show Nil = ""

safeHead :: SafeList t NonEmptyList -> t
safeHead (Cons x _) = x

example1 = safeHead (Cons 2 Nil)
{--
Does not compile
example2 = safeHead Nil
--}

safeDrop :: Int -> SafeList t NonEmptyList -> SafeList t NonEmptyList
safeDrop 0 list = list
safeDrop n list@(Cons element Nil) = list -- TODO problematic
safeDrop n (Cons element rest@(Cons _ _)) = safeDrop (n - 1) rest

safeAppend :: t -> SafeList t k -> SafeList t NonEmptyList
safeAppend element Nil = Cons element Nil
safeAppend element (Cons element' Nil) = Cons element' (Cons element Nil)
safeAppend element (Cons element' rest) = Cons element' (safeAppend element rest)
