module Exercise02.Fib where

import Test.QuickCheck

-- naive, recursive implementation
fib 0 = 0
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2)

-- faster, 'iterative' implementation
fib' n = fib_iter n 0 1
fib_iter 0 prev curr = prev
fib_iter n prev curr = fib_iter (n - 1) curr (curr + prev)

{-- TODO forAll is ambigous
-- test fib' against fib (limit the size of n)
pork_testFib n = n >= 0 ==> fib' n == fib n
tests_fib = forAll (resize 22 arbitrary) pork_testFib
--}