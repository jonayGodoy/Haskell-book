module NumbersIntoWords where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "this method only works with numbers between 0-9"

digits :: Int -> [Int]
digits n = map (\x -> (read [x])::Int) (show n)

{-recursive digit
digits :: Int -> [Int]
digits 0 = []
digits n = digits r ++ [x]
  where (r, x) = divMod n 10
-}

wordNumber :: Int -> String
wordNumber n =  concat . (intersperse "-") . (map digitToWord) . digits $ n

