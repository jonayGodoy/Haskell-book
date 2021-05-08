module Fibonacci where

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci(x - 1) + fibonacci(x - 2)


fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x