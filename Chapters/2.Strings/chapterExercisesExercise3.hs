module ChapterExercisesExercise3 where

main :: String -> IO()
main x = putStrLn [(thirdLetter x)]

thirdLetter :: String -> Char
thirdLetter x = x !! 2