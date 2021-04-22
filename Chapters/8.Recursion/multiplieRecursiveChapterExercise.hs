module MultiplieRecursiveChapterExercise where

multiplication :: Integral a => a -> a -> a
multiplication x y = go x y 0 0
 where go x y acumulator count
          | count == y = acumulator
          | otherwise = go x y (acumulator + x)(count + 1)
