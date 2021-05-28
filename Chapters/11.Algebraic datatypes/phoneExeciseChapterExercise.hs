module PhoneExeciseChapterExercise where

import Data.List;
import Data.Maybe;
import Data.Char;
import Data.String;

-- 1. Create a data structure that captures the phone layout above.
type PhoneValues = String


data DaPhone = DaPhone [(Digit, PhoneValues)] deriving (Eq, Show)

daphone = DaPhone 
   [('1', "1"),
   ('2', "abc2"),
   ('3', "def3"),
   ('4', "ghi4"),
   ('5', "jkl5"),
   ('6', "mno6"),
   ('7', "pqrs7"),
   ('8', "tuv8"),
   ('9', "wxyz9"),
   ('*', "*^"),
   ('0', " +_0"),
   ('#', "#.,")]

-- 2. Convert the following conversations into the keypresses required to express them.

convo :: [String]
convo =
 ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Just making sure rofl ur turn"]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int


reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone phone) char 
 | isUpper char = [('*',(1))] ++ (reverseTaps (DaPhone phone) (toLower char))
 | otherwise = extractTap (head (filter (\(digit, phoneValue) -> any (==char) phoneValue) phone)) char
           where extractTap (digit, phoneValue) char = [(digit, ((fromJust (elemIndex char phoneValue)) + 1))]

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]


cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone xs = foldr (\x y -> x ++ y) [] (map (\x -> reverseTaps phone x) xs)

-- 3. How many times do digits need to be pressed for each message?

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps xs = foldr (\(x',y') y -> y' + y) 0 xs

-- 4. What was the most popular letter for each message?

mostPopularLetter :: String -> Char
mostPopularLetter t@(x:_) = foldr (\x' y' -> if ((countChar t x') > (countChar t y')) then x' else y') x t
 where countChar [] y = 0
       countChar _ ' ' = 0
       countChar (x:xs) y =  if x == y then 1 + (countChar xs y) else 0 + (countChar xs y)

-- 5. What was the most popular letter overall? What was the most popular word?

coolestLtr :: [String] -> Char
coolestLtr xs = mostPopularLetter  (foldr (\x y -> x ++ y) [] xs)

coolestWord :: [String] -> String
coolestWord = mostPopularWord . words . (foldr (\x y ->  x ++ " " ++ y) [])

mostPopularWord :: [String] -> String
mostPopularWord t@(x:_) = foldr (\x' y' -> if ((countWords t x') > (countWords t y')) then x' else y') x t
 where countWords [] y = 0
       countWords (x:xs) y =  if x == y then 1 + (countWords xs y) else 0 + (countWords xs y)

