module ChapterExercises where

-- 1 fix it
data Person = Person Bool
 deriving (Show)

printPerson :: Person -> IO()
printPerson person = putStrLn (show person)

-- 2 fix it
data Mood = Blah
          | Woot deriving (Show, Eq)

settleDown x = if x == Woot
                   then Blah
                   else x
 
 
 -- 4 fix it
 
type Subject = String
type Verb = String
type Object = String
 
data Sentence =
 Sentence Subject Verb Object
 deriving (Eq, Show)
  
s1 = Sentence "dogs" "drool" "ball"
s2 = Sentence "Julie" "loves" "dogs"


myX = 1 :: Int
sigmund' :: Num a => a -> a
sigmund' x = myX

--Type-Kwon-Do Two: Electric Typealoo

