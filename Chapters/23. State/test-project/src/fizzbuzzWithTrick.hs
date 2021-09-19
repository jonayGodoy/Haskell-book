module FizzbuzzWithTrick where

import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 3 == 0 = "Fizz"
           | otherwise = show n

main :: IO ()
main = mapM_ (putStrLn . fizzBuzz) [1..100]



---------------------------------------------
-- with State

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []


fizzbuzzList' :: [Integer] -> ((), [String])
fizzbuzzList' list = runState (mapM_ addResult list) ["Jonay"]

addResult :: Integer -> State [String] ()
addResult n = do
 xs <- get
 let result = fizzBuzz n
 put (result : xs)

main' :: IO ()
main' = mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]


--mapM_ is useful for executing something only for its side effects.
-- For example, printing a string to standard output doesn't return anything useful
-- - it returns (). If we have a list of three strings,
-- we would end up accumulating a list[(), (), ()].
-- Building this list has a runtime cost, both in terms of speed and memory usage,
-- so by using mapM_ we can skip this step entirely.

-- map  | map (\x -> [x]) [0, 1, 2]      = [[0],[1],[2]]
-- mapM | mapM (\x -> [x]) [0, 1, 2]     = [[0,1,2]]
-- sequenceA $ map (\x -> [x]) [0, 1, 2] = [[0,1,2]]