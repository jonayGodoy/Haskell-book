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


--personal extra exercise, do words again
words2 :: String -> [String]
words2 [] = []
words2 xs = [takeWhile (/=' ') xs] ++  words2 (dropWhile (==' ') $ dropWhile (/=' ') xs)

takeWhile2 :: (Char -> Bool) -> String -> String
takeWhile2 f (y:ys) 
 | not (f y) = y : takeWhile2 f ys
 | f y = ""
 --- finished personal exercise


capitalizeParagraph :: String -> String
capitalizeParagraph =  plainText . plainParagraph . extractParagraph . capitalizeWords

plainText xs = f $ concat xs
 where f (x:[]) = x
       f (x:xs) = x ++ " " ++ f xs

plainParagraph = map (\(x:xs) -> snd x : (map (\(x',y') -> x') xs) )

isLastPoint (x,y) = last x /= '.'

extractParagraph :: [(String, String)] -> [[(String, String)]]
extractParagraph [] = []
extractParagraph xs = (takeWhile isLastPoint xs ++ [dropWhile isLastPoint xs !! 0]) 
                        : extractParagraph (drop 1 (dropWhile isLastPoint xs))
 
 
 
