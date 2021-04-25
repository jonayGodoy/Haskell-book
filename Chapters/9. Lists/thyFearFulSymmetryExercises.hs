module ThyFearFulSymmetryExercises where

sentence = "sheryl wants fun"

myWords :: String -> [String]
myWords x = split ' ' x


split :: Char -> String -> [String]
split x y = go x y []
 where go character text acumulator 
        | (text == "") = acumulator
        | otherwise = go character (dropFirstMatch text) (takeFirstMatch text)
         where takeFirstMatch text' = (acumulator ++ [takeWhile (/=character) text'])
               dropFirstMatch text' = dropWhile (==character) (dropWhile (/=character) text')



firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful\
\ symmetry?"

sentences = firstSen ++ secondSen
 ++ thirdSen ++ fourthSen


shouldEqual = [ 
  "Tyger Tyger, burning bright",
  "In the forests of the night", "What immortal hand or eye", 
  "Could frame thy fearful symmetry?"
 ]

myLines :: String -> [String]
myLines x = split '\n' x


main :: IO ()
main =
 print $
  "Are they equal? "
  ++ show (myLines sentences
   == shouldEqual)