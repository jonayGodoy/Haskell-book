module ChapterExerciseConsonantsAndVowels where

--stops = "pbtdkg"
--vowels = "aeiou"

--1. Write a function that takes inputs from stops and
-- vowels and makes 3-tuples of all possible stop-vowelstop
-- combinations. These will not all correspond to
-- real words in English, although the stop-vowel-stop
-- pattern is common enough that many of them will.


stops = "pbtdkg"
vowels = "aeiou"

stopVowelStop :: [(Char, Char, Char)]
stopVowelStop = [(x,y,z) | x <- stops , y <- vowels , z <- stops]

-- Modify that function so that it only returns the combinations that begin with a p.

stopVowelStopJustP :: [(Char, Char, Char)]
stopVowelStopJustP = [(x,y,z) | 
                      x <- stops , 
                      y <- vowels , 
                      z <- stops, 
                      x == 'p', 
                      y == 'a']
