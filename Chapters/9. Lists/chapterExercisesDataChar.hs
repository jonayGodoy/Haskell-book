module ChapterExercisesDataChar where

import Data.Char

justUpperCase :: String -> String
justUpperCase xs = filter isUpper xs

capitalizeFirstLetter :: String -> String
capitalizeFirstLetter (x:xs) = toUpper x : xs

recursiveToUpper :: String -> String
recursiveToUpper [] = []
recursiveToUpper (x:xs) = toUpper x : recursiveToUpper xs

headToUpper1 :: String -> Char
headToUpper1 (x:xs) = toUpper x

headToUpper2 :: String -> Char
headToUpper2 xs = toUpper $ head xs

headToUpper3 :: String -> Char
headToUpper3 = toUpper . head 