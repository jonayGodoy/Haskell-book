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

--no se por que el 4, que funciona igual que el 3 con concatenacion(.)
-- no consigo lo dejo inacabado por si en el futuro puedo volver y corregirlo
--tensDigit4 :: Integral a => a -> a
--tensDigit4 x =  snd . divMod ((fst . (divMod (10 10))) 10)
--tensDigit4 x =  snd . divMod ((fst (divMod x 10)) 10)

 