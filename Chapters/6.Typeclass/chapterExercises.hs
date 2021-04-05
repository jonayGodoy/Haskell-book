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
sigmund' :: Int -> Int
sigmund' x = myX

--Type-Kwon-Do Two: Electric Typealoo
--1
--chk :: Eq b => (a -> b) -> a -> b -> Bool
--chk = ???
chk x y z = x(y) == z

--2

--arith :: Num b
--      => (a -> b)
--      -> Integer
--     -> a
--      -> b
--arith = ???
arith x y z = x(z) + fromInteger y

