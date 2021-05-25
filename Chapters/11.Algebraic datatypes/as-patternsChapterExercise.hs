module AsPatternsChapterExercise where

import Data.Char
import Data.List
import Data.Maybe

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf (x:xs) ys = isAny x ys && isSubseqOf xs (drop (findPosition x ys) ys)
 where isAny = any . (==)
       findPosition x xs = fromJust $ elemIndex x xs

--copy of internet, i didn't find solution
isSubseqOf2 :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf2 [] _ = True
isSubseqOf2 _ [] = False
isSubseqOf2 t@(x:xs) (y:ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf t ys


capitalizeWords :: String -> [(String, String)]
capitalizeWords = (map f ) . words
 where f t@(x:xs) = (t, (toUpper x : xs))


