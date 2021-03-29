module PatternMatchAreNonExhaustive where

f :: Int -> Bool
f 1 = True
f 2 = True
f 3 = True
f _ = False --Pattern match(es) are non-exhaustive