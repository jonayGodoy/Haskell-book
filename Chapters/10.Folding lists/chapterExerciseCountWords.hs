module ChapterExerciseCountWords where


seekritFunc x = div (sum (map length (words x))) (length (words x))

--We’d really like the answer to be more precise. 
--Can you rewrite that using fractional division?

seekritFunc2 :: String -> Double
seekritFunc2 x = (/) 
  (fromIntegral((sum (map length (words x)))))
  (fromIntegral ((length (words x))))
