module ChapterExercisesExercise4 where

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

main :: Int -> IO()
main x = putStrLn [(letterIndex x-1)]