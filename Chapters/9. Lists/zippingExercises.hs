module ZippingExercises where

zip1 :: (Eq a, Eq b)=> [a] -> [b] -> [(a, b)]
zip1 xs xs' =  [ (x, y) | 
  x <- xs,
  y <- xs',
  z <- enumFromTo 0 ((min (length xs) (length xs'))-1),
  x == (xs !! z) && y == (xs' !! z)
 ]



zip2 :: [a] -> [b] -> [(a, b)]
zip2 xs xs' =  go xs xs' 0 []
 where go xs xs' index acumulator
         | (length xs == index || length xs' == index) = acumulator
         | otherwise = go xs xs' (index+1) (acumulator ++ [(xs !! index, xs' !! index)])

genericZip :: (a -> b -> c) -> [a] -> [b] -> [c]
genericZip f xs xs' =  go f xs xs' 0 []
 where go f xs xs' index acumulator
         | (length xs == index || length xs' == index) = acumulator
         | otherwise = go f xs xs' (index+1) (acumulator ++ [f (xs !! index) (xs' !! index)])

zip4 :: [a] -> [b] -> [(a, b)]
zip4 xs xs' = genericZip (\x y -> (x,y)) xs xs'