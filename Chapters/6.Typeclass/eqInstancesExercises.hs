{-# LANGUAGE NoMonomorphismRestriction #-}
module EqInstanceExercices where

--1 exercise
data TisAnInteger = TisAn Integer

instance Eq a b => (Eq a) (Integer b) where
  (==) (TisAn Integer) (TisAn' Integer') = 
        TisAn == TisAn' && Integer == Integer

   