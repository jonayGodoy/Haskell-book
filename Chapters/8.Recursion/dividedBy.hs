module DividedBy where

data DividedResult =  Result (Integer, Integer) | DividedByZero deriving (Show)

dividedBy :: Integer -> Integer -> DividedResult
dividedBy 0 _ = DividedByZero
dividedBy _ 0 = DividedByZero
dividedBy num denom = go num denom 0 (denom < 0 && num < 0 || denom >= 0 && num >= 0)
 where go n d count sign
        | (abs n) < (abs d) && not sign = Result (-count, n)
        | (abs n) < (abs d) && sign = Result (count, n)
        | otherwise = go ((abs n) - (abs d)) d (count + 1) sign


