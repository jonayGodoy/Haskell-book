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


newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Eq a => EqProp (Identity a) where
 (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
 arbitrary = do
  x <- arbitrary
  return (Identity x)

instance Functor Identity where
 fmap f (Identity x) = Identity (f x)


instance Applicative Identity where
 pure x = Identity x
 Identity f <*> Identity x  = Identity (f x)


instance Monad Identity where
 return = pure
 Identity x >>= f = f x

identity = undefined :: Identity SSI



data List a = Nil | Cons a (List a) deriving (Show, Eq)

instance Arbitrary a => Arbitrary (List a) where
 arbitrary = do
  x <- arbitrary
  return $ Cons x Nil

instance Eq a => EqProp (List a) where
 (=-=) = eq

instance Functor List where
 fmap _ Nil = Nil
 fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
 pure x = Cons x Nil
 Nil <*> _                   = Nil
 _  <*> Nil                 = Nil
 (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)


instance Monoid (List a) where
  mempty = Nil
  
instance Semigroup (List a) where  
  Nil <> ys         = ys
  (Cons x xs) <> ys = Cons x (xs <> ys)



instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x xs >>= f = f x `mappend` (xs >>= f)


list = undefined :: List SSI

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
 putStrLn "\n3. Identity"
 quickBatch $ functor identity
 quickBatch $ applicative identity
 quickBatch $ monad identity
 putStrLn "\n4. List"
 quickBatch $ functor list
 quickBatch $ applicative list
 quickBatch $ monad list
