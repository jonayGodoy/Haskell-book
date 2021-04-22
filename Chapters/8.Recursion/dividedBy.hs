module DividedBy where

data DividedResult =  Result (Integer, Integer) | DividedByZero deriving (Show)

dividedBy :: Integer -> Integer -> DividedResult
dividedBy num denom = go num denom 0 (denom < 0 && num < 0 || denom >= 0 && num >= 0) (num == 0 || denom == 0)
 where go n d count sign hasZero
        | hasZero = DividedByZero
        | (abs n) < (abs d) && not sign = Result (-count, n)
        | (abs n) < (abs d) && sign = Result (count, n)
        | otherwise = go ((abs n) - (abs d)) d (count + 1) sign hasZero


