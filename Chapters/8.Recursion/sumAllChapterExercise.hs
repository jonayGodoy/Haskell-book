module SumAllChapterExercise where

sumAll :: Integral a => a -> a
sumAll 0 = 0
sumAll x = x + sumAll(x - 1)