module Type_Kwon_DoExercise4 where

munge :: (x -> y)
 -> (y -> (w, z))
 -> x
 -> w
munge a b c = fst ( b (a c)) 

--munge = ????