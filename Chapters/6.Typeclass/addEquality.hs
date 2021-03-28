{-# LANGUAGE NoMonomorphismRestriction #-}
module AddEquality where

data Trivial = Trivial' 

instance Eq Trivial where
 Trivial' == Trivial'  = True