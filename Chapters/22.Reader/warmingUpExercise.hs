module WarmingUpExercise where


import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs


rev :: [Char] -> [Char]
rev xs = reverse xs


composed :: [Char] -> [Char]
composed = cap . rev


fmapped :: [Char] -> [Char]
fmapped = cap <$> rev


--We will want to use an Applicative here
tupled :: [Char] -> ([Char], [Char])
tupled = (,) <$> cap <*> rev

-- There is no special reason such a function needs to be
-- monadic, but let’s do that, too, to get some practice. Do it
-- one time using do syntax; then try writing a new version using (>>=)
tupled' :: [Char] -> ([Char], [Char])
tupled' = do
 x <- cap
 y <- rev
 return (x, y)














