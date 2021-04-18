module ChapterExercisesTensDigit where


tensDigit :: Integral a => a -> a
tensDigit x = d
 where xLast = x `div` 10
       d = xLast `mod` 10


tensDigit2 :: Integral a => a -> a
tensDigit2 x = d
      where (xLast, _) = x `divMod` 10
            (_, d) = xLast `divMod` 10



tensDigit3 :: Integral a => a -> a
tensDigit3 x =  snd (divMod (fst (divMod x 10)) 10)

tensDigit35 :: Integral a => a -> a
tensDigit35 x =  snd (last (fst (last x )))
 where last x = x `divMod` 10

-- i'm few worry cause function composition reading inversed to normal reading
tensDigit4 :: Integral a => a -> a
tensDigit4 x =  snd . last . fst . last $ x 
 where last x = x `divMod` 10
 