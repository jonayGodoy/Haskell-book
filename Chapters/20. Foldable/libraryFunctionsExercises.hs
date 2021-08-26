module LibraryFunctionsExercises where

import Data.Foldable
import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' xs = getSum $ foldMap Sum xs


product' :: (Foldable t, Num a) => t a -> a
product' xs = getProduct $ foldMap Product xs


elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x' xs = foldr (\x y -> x == x') False xs


minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr go Nothing
  where
    go x Nothing  = Just x
    go x (Just y) = Just (min x y)


maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr go Nothing
  where
    go x Nothing  = Just x
    go x (Just y) = Just (max x y)


null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\x y -> y + 1) 0


toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\x y -> x : y) []

--copy solution, this was a surprise was really simple
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id


--copy solution
foldMap'' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap'' f = foldr (mappend . f) mempty








