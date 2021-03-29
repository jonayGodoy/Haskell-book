{-# LANGUAGE NoMonomorphismRestriction #-}
module GenericType where

data Identity a = Identity a

--instance Eq (Identity a) where
-- (==) (Identity v) (Identity v') = v == v'

--instance Eq a => Eq (Identity a) where
 --(==) (Identity v) (Identity v') = v == v'
 
instance Ord a => Eq (Identity a) where
 (==) (Identity v) (Identity v') = compare v v' == EQ