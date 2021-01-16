module ChapterExercisesExercise2 where

main :: [Char] -> IO()
main x = do
 putStrLn (exerciseAAddExclamaxion x)
 putStrLn (exerciseBReturnY x)
 putStrLn (exerciseCDrop9 x)
 where exerciseAAddExclamaxion x = concat[x, "!"]
       exerciseBReturnY x = "y"
       exerciseCDrop9 x = drop 9 (exerciseAAddExclamaxion x)