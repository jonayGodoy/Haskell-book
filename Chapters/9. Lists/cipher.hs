module Cipher where

import Data.Char


displaceChar :: Char -> Int -> Char
displaceChar char displacement
   | not . isletter $ char = char
   | isHighLimitExceed extractedPosition = chr (extractedPosition - highLimit + lowerLimit - 1)
   | islowerLimitExceed extractedPosition = chr (extractedPosition + highLimit - lowerLimit + 1)
   | otherwise = chr extractedPosition
   where highLimit = 122
         lowerLimit = 97
         isHighLimitExceed position = position > highLimit
         islowerLimitExceed position = position < lowerLimit
         isletter char = lowerLimit <= (ord (toLower char)) && (ord (toLower char)) <= highLimit  
         extractedPosition = (ord (toLower char)) + (rem displacement 27)
--a 97, z 122

caesarCipher :: String -> Int -> String
caesarCipher "" _ = ""
caesarCipher " " _ = " "
caesarCipher text 0 = text
caesarCipher text displacement = map (\x -> displaceChar x displacement) text

unCaesarCipher :: String -> Int -> String
unCaesarCipher "" _ = ""
unCaesarCipher " " _ = " "
unCaesarCipher text 0 = text
unCaesarCipher text displacement = map (\x -> displaceChar x (negate displacement)) text
