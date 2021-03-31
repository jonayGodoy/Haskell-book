{-# LANGUAGE NoMonomorphismRestriction #-}
module EqInstanceExercices where

--1 exercise
data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn x) (TisAn x') = x == x'

--Como definiste TisAnInteger, tu constructor de datos es TisAn, noTisAnInteger
--data TisAnInteger = TisAn Integer
--      [1]          [2]
--1. Constructor de tipos
--2. Constructor de datos

--2 exercise
data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
 (== )(Two a b) (Two a' b') = a == a' && b == b'

--3 exercise
data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
 (==) (TisAnInt a) (TisAnInt a') = a == a'
 (==) (TisAString a) (TisAString a') = a == a'
 (==) _ _ = False

--4 exercise

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
 (==) (Pair x y) (Pair x' y') = x == x' && y == y'


--exercise 5
data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
 (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

-- exercise 6
data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
 (==) (ThisOne a) (ThisOne a') = a == a'
 (==) (ThatOne b) (ThatOne b') = b == b'
 (==) _ _ = False

-- exercise 7
data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  Hello a == Hello a' = a == a'
  Goodbye b == Goodbye b' = b == b'
  _ == _ = False
