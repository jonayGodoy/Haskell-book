module SemigroupChapterExercises where

import Data.Monoid
import Test.QuickCheck

-- 1 .

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
 _ <> _ = Trivial
 
instance Arbitrary Trivial where
 arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool


--2
newtype Identity a = Identity a deriving (Show, Eq)


instance Arbitrary a => Arbitrary (Identity a) where
 arbitrary = do
  x <- arbitrary
  return (Identity x)


type IdentityAssoc = Identity Trivial -> Identity Trivial -> Identity Trivial -> Bool

instance Semigroup a => Semigroup (Identity a) where
 (Identity x) <> (Identity y) = Identity (x <> y)

--3

data Two a b = Two a b





main :: IO ()
main = do
 --1.
  quickCheck (semigroupAssoc :: TrivAssoc)
 --2.
  quickCheck (semigroupAssoc :: IdentityAssoc)