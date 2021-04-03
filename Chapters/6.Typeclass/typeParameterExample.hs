module TypeParameterExample where

--add :: a -> a -> a
add :: Num a => a -> a -> a
add x y = x + y


addWeird :: (Ord a, Num a) => a -> a -> a
addWeird x y =
 if x > 1
 then x + y
 else x
 
--- extra aren't in the book
--- multiples parameters, multiples types

printWeird :: Int -> String -> IO() 
printWeird x y = print (show (x) ++ y)

--- multiples parameters, multiples typeclass
eqWeird :: (Eq a, Show a) => a -> String -> Bool 
eqWeird x y = show (x) == y
