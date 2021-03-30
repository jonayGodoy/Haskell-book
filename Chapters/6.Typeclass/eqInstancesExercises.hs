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

