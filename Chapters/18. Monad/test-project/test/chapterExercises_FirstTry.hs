module ChapterExercises_FirstTry where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type SSI = (String, String, Int)

data Nope a = NopeDotJpg deriving (Show, Eq)

instance Functor Nope where
 fmap _ _ = NopeDotJpg

instance Applicative Nope where
 pure _ = NopeDotJpg
 _ <*> _ = NopeDotJpg

instance Monad Nope where
 return = pure
 _ >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
 arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
 (=-=) = eq


nope = undefined :: Nope SSI


data PhhhbbtttEither b a = Left' a | Right' b deriving (Show, Eq)

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
 arbitrary = do
  x <- arbitrary
  y <- arbitrary
  elements [Left' y, Right' x]

instance (Eq b, Eq a) => EqProp (PhhhbbtttEither b a) where
 (=-=) = eq

instance Functor (PhhhbbtttEither b) where
 fmap f (Left' x) = Left' (f x)
 fmap _ (Right' x) = Right' x

instance Applicative (PhhhbbtttEither b) where
 pure x = Left' x
 Right' x <*> _      = Right' x
 _ <*> Right' x      = Right' x
 Left' f <*> Left' x = Left' (f x)

instance Monad (PhhhbbtttEither b) where
 return = pure
 Right' x >>= _ = Right' x
 Left' x >>= f = f x

peither = undefined :: PhhhbbtttEither String SSI

main :: IO ()
main = do
 putStrLn "\n1. Nope"
 quickBatch $ functor nope
 quickBatch $ applicative nope
 quickBatch $ monad nope
 putStrLn "\n2. PhhhbbtttEither"
 quickBatch $ functor peither
 quickBatch $ applicative peither
 quickBatch $ monad peither
 
