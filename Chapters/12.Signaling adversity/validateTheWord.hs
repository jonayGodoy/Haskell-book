module ValidateTheWord where

import Data.Maybe;

vowels = "aeiou"

consonats = filter (\x -> not (any (x==) vowels)) ['a'..'z']

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord xs = if countVowels xs < countConsonants xs then Just (Word' xs) else Nothing

countVowels :: String -> Integer
countVowels xs = countIsAny xs isVowel
 where isVowel x = any (==x) vowels

countConsonants :: String -> Integer
countConsonants xs = countIsAny xs isConsonant
 where isConsonant x = not (any (==x) vowels)

countIsAny :: String -> (Char -> Bool) -> Integer
countIsAny xs f = toInteger $ length $ filter f xs