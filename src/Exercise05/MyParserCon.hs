module Exercise05.MyParserCon where

-- Language definition
data Term = Constant Integer |
            Binary Term Op Term
            deriving (Show, Eq)

data Op = Add | Sub | Mul | Div
          deriving (Show, Eq)

-- Primitive parser
-- Consumes all characters at once
type VanillaParser token result = [token] -> result

-- Accepts [Char] == String and returns a Term
type TermParser = VanillaParser Char Term

-- Can be invoked recursively
-- Not all characters must be consumed at once
-- But no multiple choices
type VanillaParser2 token result = [token] -> (result, [token])

-- This is the real shit
-- Doesn't consume all the characters at once
-- Return multiple ways of parsing (list of successes technique)
type Parser token result = [token] -> [(result, [token])]

-- If the list of tokens is empty (r, []),
-- the parser was able to consume all the characters
-- in the input and the final result is r

-- Primitive parsers (for combinator parsing)

-- Recognizes the empty language
-- We don't have anything to recognize
-- so we return an empty list
pempty :: Parser t r
pempty tokens = []

-- Recognizes the empty word
-- We leave the input untouched
-- r -> (Parser t) r looks like a monad return function
succeed :: r -> Parser t r
succeed result tokens = [(result, tokens)]

-- Recognizes token if predicate holds
satisfy :: (t -> Bool) -> Parser t t
satisfy predicate (t:ts) | predicate t = succeed t ts
satisfy _ _ = []

-- Same as satisfy but transforms the token directly
-- transform is a function that may map the token
msatisfy :: (t -> Maybe a) -> Parser t a
msatisfy transform (t:ts) = case transform t of
    Just result -> succeed result ts
    Nothing -> []
msatisfy _ _ = []

-- Recognizes if the first token is equal to t
lit :: Eq t => t -> Parser t t
lit t tokens = satisfy (== t) tokens

-- Parser combination

-- Recognizes union of two languages
-- Just merge the result lists
-- p1 biased
palt :: Parser t r -> Parser t r -> Parser t r
palt p1 p2 tokens = (p1 tokens) ++ (p2 tokens)

-- Recognizes the concatenation of two languages (sequence)
pseq :: Parser t (s -> r) -> Parser t s -> Parser t r
pseq p1 p2 tokens = concat lists
    where list1 = p1 tokens -- [(s -> r, [t])]
          lists  = map mapList list1
          mapList (sToR, ts) = map (applyFunc sToR) (p2 ts) -- [s, [t]]
          applyFunc sToR (s, ts) = (sToR s, ts)

-- Recognizes the language of parser but maps the results
pmap :: (s -> r) -> Parser t s -> Parser t r
pmap func parser tokens = map mapFunc (parser tokens)
    where mapFunc (s, ts) = (func s, ts)

-- Examples

exampleParser = pseq (pmap (\c -> \d -> c) (lit 'a')) (lit 'b')

{--
Example grammar
A -> aA
A -> aB
B -> b
--}

-- Return a string that was read
pA :: Parser Char String
pA = palt
    (pseq (pmap (:) (lit 'a')) pA)
    (pseq (pmap (:) (lit 'a')) pB)

pB :: Parser Char String
pB = pmap (const "b") (lit 'b')


-- Parser is also a functor
data ParserType token result = ParserType {
        executeParser :: Parser token result
    }

instance Functor (ParserType token) where
    fmap f (ParserType p) = ParserType (pmap f p)