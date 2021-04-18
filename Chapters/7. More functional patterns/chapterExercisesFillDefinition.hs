module ChapterExercisesFillDefinition where


g :: (a -> b) -> (a, c) -> (b, c)
--g = undefined
g x (y, z) = (x y, z)