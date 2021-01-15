module ChapterExercisesExercise5 where

x = "Curry is awesome"

rvrs :: String
rvrs =  extractThirdWord x ++ " " ++ extractSecondWord x ++ " " ++ extractFirstWord x
 where extractFirstWord x = take 5 x
       extractSecondWord x = drop 6 (take 8 x)
       extractThirdWord x = drop 9 x
 
main :: IO()
main = putStrLn rvrs