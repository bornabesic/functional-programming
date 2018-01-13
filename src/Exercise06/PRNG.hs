module Exercise06.PRNG where

import Control.Monad.Trans.State.Lazy

-- State monad with:
--  state :: Int
--  value :: a (whatever)
type Random a = State Int a

-- Returns a fresh random number
-- Depends on the internal state
fresh :: Random Int
fresh = do
    current <- get
    let next = lcg current
    put next
    return next -- slightly different from the official solution: do not start with the initial state


-- Linear congruential generator proposed by Donald Knuth
-- x(n) = (6364136223846793005 ∗ x(n−1) + 1442695040888963407) mod 2^64
lcg :: Int -> Int
lcg x = fromIntegral ((6364136223846793005 * fromIntegral x + 1442695040888963407) `mod` 2^64)

-- Used for running the (Random a) monads
-- Returns the final value
-- evalState rand init == fst $ (runState rand) init
runPRNG :: Random a -> Int -> a
runPRNG rand init = evalState rand init

-- Usage examples (runPRNG example initialState)
example1 = do
    firstRandomNumber <- fresh
    return firstRandomNumber

example2 = do
    firstRandomNumber <- fresh
    secondRandomNumber <- fresh
    return secondRandomNumber

-- Sum of n random numbers
randomSum :: Int -> Random Int
randomSum 0 = return 0
randomSum n = do
    new <- fresh
    another <- randomSum (n-1)
    return (new + another)

-- Generate a list of n random numbers
randomList :: Int -> Random [Int]
randomList 0 = return []
randomList n = do
    new <- fresh
    others <- randomList (n-1)
    return (new:others)

