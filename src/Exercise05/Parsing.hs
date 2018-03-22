module Exercise05.Parsing where

import Data.Char

import Exercise05.ParserCon

-- 1+
psome :: Parser t r -> Parser t [r]          
psome p = pure (:)
          <*> p -- at least once
          <*> pmany p -- zero or more (doesn't matter)

-- 0+
pmany :: Parser t r -> Parser t [r]
pmany p = psome p -- one or more
          <|>
          pure [] -- zero

-- Recognizes an integer
pInt :: Parser Char Integer
pInt = pure read -- read :: String -> Integer
       <*> psome (satisfy isDigit) -- [Char] = String

-- Recognizes a double (exercise)       
pDouble :: Parser Char Double
pDouble = pure fromIntegral
          <*> pInt 
          <|>
          pure (\a -> \b -> \c -> read $ a ++ [b] ++ c)
          <*> psome (satisfy isDigit)
          <*> lit '.'
          <*> psome (satisfy isDigit)

-- Recognizes integer lists in Haskell syntax
pIntList :: Parser Char [Integer]
pIntList = pure (++)
           <* lit '['
           <*> pmany pInt
           <*> pmany (lit ',' *> pInt)     
           <* lit ']'

{--

TODO:

pPaliAB :: Parser Char String

pPali :: (Eq r) => Parser t r -> Parser t [r]

pTwice :: (Eq t) => Parser t [t] -> Parser t [t]

--}