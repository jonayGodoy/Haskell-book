module EnumFromToExercise where

eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool True False = []

eftOrd :: Ordering
       -> Ordering
       -> [Ordering]
eftOrd x y = eft x y

eftInt :: Int -> Int -> [Int]
eftInt x y = eft x y

eftChar :: Char -> Char -> [Char]
eftChar x y = eft x y

eft:: (Eq a, Enum a) => a -> a -> [a]
eft x y = go x y x [x]
 where go x y pointer acumulator
        | (pointer == y) = acumulator
        | otherwise = go x y (succ pointer) (acumulator ++ [succ pointer])