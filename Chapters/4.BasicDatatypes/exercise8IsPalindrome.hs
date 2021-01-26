module Exercise8IsPalindrome where

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse(x)