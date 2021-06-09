module VigenereCipherIOChapterExercise where

import Data.Char
import Data.List
import Data.Maybe
import System.IO


main :: IO ()
main = do
 hSetBuffering stdout NoBuffering
 putStr "Please introduce text to cipher:"
 text <- getLine
 putStr "Please introduce text to keyword:"
 keyword <- getLine
 putStrLn $ vigenereCipher text keyword


keyword = "ally"

sentence = "MEET AT DAWN"

-- ALLY AL LYAL

expect = "MPPR AE OYWY"

lowerAbecedary = ['a'..'z'];

upperAbecedary = ['A'..'Z']

vigenereCipher :: String -> String -> String
vigenereCipher sentence keyword = map (\(x, y) -> cipherLetter x y) (zip sentence (processKeyword sentence keyword))


processKeyword :: String -> String -> String
processKeyword sentence keyword = go sentence keyword keyword
 where go [] _ _ = []
       go (' ':xs) keyword originKeyword = [' '] ++ go xs keyword originKeyword
       go (x:xs) [] (z:zs) = [z] ++ go xs zs (z:zs) 
       go (x:xs) (y:ys) originKeyword = [y] ++ go xs ys originKeyword


cipherLetter :: Char -> Char -> Char
cipherLetter ' ' key = ' '
cipherLetter char key = upperAbecedary !! (extractPosition (extractNumber char) (extractNumber key))

extractPosition :: Int -> Int -> Int
extractPosition x y
 | x + y >= length upperAbecedary = x + y - (length upperAbecedary)
 | x + y < length upperAbecedary = x + y

extractNumber :: Char -> Int
extractNumber x 
 | isLower x = fromJust $ elemIndex x lowerAbecedary
 | isUpper x = fromJust $ elemIndex x upperAbecedary
 
test = if vigenereCipher sentence keyword == expect then "Success" else "failure"