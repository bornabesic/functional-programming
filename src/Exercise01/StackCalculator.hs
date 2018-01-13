module Exercise01.StackCalculator where

import Data.List

initStack :: [Int]
initStack = repeat 0

opError op = error ("There are no enough elements on the stack to perform " ++ op)

pop :: [Int] -> [Int]
pop (top:stack) = stack
pop [] = []

push :: [Int] -> Int -> [Int]
push stack number = number : stack

add :: [Int] -> [Int]
add (a:b:stack) = (a + b) : stack
add _ = opError "add"

dup :: [Int] -> [Int]
dup (top:stack) = (2 * top) : stack
dup _ = opError "dup"

substract :: [Int] -> [Int]
substract (a:b:stack) = (a - b) : stack
substract _ = opError "substract"

multiply :: [Int] -> [Int]
multiply (a:b:stack) = (a * b) : stack
multiply _ = opError "multiply"

neg :: [Int] -> [Int]
neg (top:stack) = (-top) : stack
neg _ = opError "neg"

peek :: [Int] -> Int
peek stack = (take 1 stack) !! 0

--

readCommand :: String -> [Int] -> [Int]
readCommand "pop" stack = pop stack
readCommand "dup" stack = dup stack
readCommand "add" stack = add stack
readCommand "substract" stack = substract stack
readCommand "multiply" stack = multiply stack
readCommand "neg" stack = neg stack
readCommand cmd stack | "push " `isPrefixOf` cmd = push stack number
                      |  otherwise = stack
                         where (prefix, rest) = splitAt 5 cmd
                               number = read rest :: Int

-- testing
prop_add x y rest = add [x, y] ++ rest == [x + y] ++ rest
prop_substract x y rest = substract [x, y] ++ rest == [x - y] ++ rest
prop_push num stack = push stack num == num : stack
prop_dup x rest = dup (x:rest) == (2 * x) : rest
prop_multiply x y rest = multiply [x, y] ++ rest == [x * y] ++ rest
prop_neg x rest = neg (x:rest) == (-x) : rest

