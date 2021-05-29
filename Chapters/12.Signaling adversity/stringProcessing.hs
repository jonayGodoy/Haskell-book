module StringProcessing where

import Data.String;

-- 1 exercise 
replaceThe :: String -> String
replaceThe xs = replaceWordsTheToA $ words xs
 where replaceWordsTheToA (x:[]) = replaceTheToA x
       replaceWordsTheToA (x:xs) = replaceTheToA x ++ " " ++ replaceWordsTheToA xs
       replaceTheToA x = if x == "the" then "a" else x

--2 exercise
countTheBeforeVowelInWords :: [String] -> Integer
countTheBeforeVowelInWords [] = 0
countTheBeforeVowelInWords ("the":xs) = (if (isVowel (head $ head xs)) then 1 else 0) + (countTheBeforeVowelInWords xs)
  where isVowel x = any (==x) ['a','e','i','o','u']
countTheBeforeVowelInWords (x:xs) = countTheBeforeVowelInWords xs

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = countTheBeforeVowelInWords . words 

-- 3 exercise

countVowels :: String -> Integer
countVowels [] = 0
countVowels (x:xs) = (if (isVowel x) then 1 else 0) + (countVowels xs)
   where isVowel x = any (==x) ['a','e','i','o','u']

countVowels2 :: String -> Integer
countVowels2 [] = 0
countVowels2 xs = toInteger $ length $ filter (\x -> isVowel x) xs
 where isVowel x = any (==x) ['a','e','i','o','u']