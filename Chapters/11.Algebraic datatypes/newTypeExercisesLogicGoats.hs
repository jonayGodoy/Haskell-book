{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module NewTypeExercisesLogicGoats where


class TooMany a where
  tooMany :: a -> Bool

--1
instance TooMany Int where
  tooMany n = n > 42

-- needs FlexibleInstances
instance TooMany (Int, String) where
  tooMany (n, n') = n > 42

--2
newtype Goats = Goats (Int, Int)
instance TooMany Goats where
  tooMany (Goats (m, n)) = (m+n)>42

--3
-- needs FlexibleInstances
instance (Num a, Ord a, TooMany a) => TooMany (a, a) where
  tooMany (n, n') = (n + n') > 42