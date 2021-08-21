module LibraryFunctionsExercises where

import Data.Foldable
import Data.Monoid

sum' :: (Foldable t, Num a) => t a -> a
sum' xs = getSum $ foldMap Sum xs