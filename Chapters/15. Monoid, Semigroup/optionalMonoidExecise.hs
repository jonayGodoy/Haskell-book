module OptionalMonoidExecise where

import Data.Monoid


data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
   mempty = Nada

--Semigroup insta is necessary because the book is obsolete
instance Semigroup a => Semigroup (Optional a) where
  Nada <> (Only a) = Only a
  (Only a) <> Nada = Only a
  (Only a) <> (Only a') = Only (a <> a')
  Nada <> Nada = Nada

main :: IO ()
main = do
  print $ Only (Sum 1) `mappend` Only (Sum 1)