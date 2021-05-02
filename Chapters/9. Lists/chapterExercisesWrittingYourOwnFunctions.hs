module WrittingYourOwnFunctions where

import Data.List

myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) =
 if x == False
  then False
  else myAnd xs

myOr :: [Bool] -> Bool
myOr [] = True
myOr (x:xs) = 
 if x == False 
  then False
  else myOr (xs)

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = 
 if (f x) 
  then True
  else myAny f (xs)

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x' (x:xs) =
 if (x' == x)
  then True
  else myElem x' xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x: xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x: xs) = (f x) ++ squishMap (f) xs

--squishAgain :: [[a]] -> [a]
--squishAgain xs = squishMap (\x -> fst x) xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = go f xs x
 where go _ [] greatest = greatest
       f (x':xs') greatest = if ((f x' greatest) == GT)
                              then go f xs' x'
                              else go f xs' greatest

