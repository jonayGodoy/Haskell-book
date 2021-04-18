module ChapterExercisesFoldBool where

--contructor 
foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

--case expression
foldBool2 :: a -> a -> Bool -> a
foldBool2 x y z =
 case z of
  True -> y
  False -> x

--guards
foldBool1 :: a -> a -> Bool -> a
foldBool1 x y z
 | z = y
 | not z = x

--if then else expression
foldBool0 :: a -> a -> Bool -> a
foldBool0 x y z = if z then y else x
