module Type_Kwon_DoExercise3 where

data X
data Y
data Z



xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

--xform :: (X, Y) -> (Z, Z)
--xform = ???
-- answer: xform (a, b) = (xz a, yz b)