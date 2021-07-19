{-# LANGUAGE FlexibleInstances #-}

module ChapterExercises where
-- Write Functor instances for the following datatypes.

-- 1

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
 fmap _ Finance = Finance
 fmap _ (Desk a) = Desk a
 fmap f (Bloor b) = Bloor (f b)

--2 No, it’s not interesting by itself.

data K a b = K a

instance Functor (K a) where
 fmap _ (K a) = K a

-- 3

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a deriving (Eq, Show)

instance Functor (Flip K' a) where
 fmap f (Flip (K' a)) = Flip (K' (f a))


-- 4

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Functor (EvilGoateeConst a) where
 fmap f (GoatyConst b) = GoatyConst (f b)


-- 5 wrap | fmap (+5) $ LiftItOut (Just 1)

data LiftItOut f a = LiftItOut (f a) deriving (Eq, Show)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- 6 

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
 fmap f' (DaWrappa fa ga) = DaWrappa (fmap f' fa) (fmap f' ga)

-- 7

data IgnoreOne f g a b = IgnoringSomething (f a) (g b) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
 fmap f (IgnoringSomething fa gb) =  IgnoringSomething fa (fmap f gb)


-- 8 
data Notorious g o a t = Notorious (g o) (g a) (g t) deriving (Eq, Show)

instance Functor g => Functor (Notorious g o a) where
 fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)


