module ChapterExerciseRewritingUsingFolds where


myOr :: [Bool] -> Bool
myOr xs = foldr (\x y -> if x then True else y) False xs

myOr2 :: [Bool] -> Bool
myOr2 xs = foldr (||) False xs


myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x y -> if f x then True else y) False xs

myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 f xs = foldr (\x y -> (f x) || y) False xs

myElem :: Eq a => a -> [a] -> Bool
myElem x xs = foldr (\x' y' -> if x' == x then True else y' ) False xs


myElem2 :: Eq a => a -> [a] -> Bool
myElem2 x xs = myAny2 (\x' -> x' == x ) xs


myReverse :: [a] -> [a]
myReverse xs = foldl (flip (:)) [] xs


myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\x y -> f x : y) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = foldr (\x y -> if f x then x : y else y) [] xs

squish :: [[a]] -> [a]
squish xs = foldr (\x y -> x ++ y) [] xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = foldr (\x y -> f x ++ y) [] xs

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap (\x -> x) xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl (\x y -> if (f x y) == GT then x else y) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (\x y -> if (f x y) == LT then x else y) x xs