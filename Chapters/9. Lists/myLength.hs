module MyLength where

mylength :: [a] -> Integer
mylength [] = 0
mylength (_:xs) = 1 + mylength xs