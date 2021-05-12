module Fibonacci where

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci(x - 1) + fibonacci(x - 2)


fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

fibsJust20 = take 20 fibs

--n * factorial (n - 1)
factorials :: Integral a => a -> [a]
factorials 0 = [1]
factorials x = scanl (\x' y -> x * y) x (factorials (x - 1))