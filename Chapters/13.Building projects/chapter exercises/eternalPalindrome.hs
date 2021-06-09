module EternalPalindrome where

import Data.Char
import Control.Monad

palindrome :: IO ()
palindrome = forever $ do
 line1 <- getLine
 case (line1 == reverse line1) of
  True -> putStrLn "It's a palindrome!"
  False -> putStrLn "Nope!"

palindrome' :: IO ()
palindrome' = do
 line1 <- getLine
 case (line1 == reverse line1) of
  True -> putStrLn "It's a palindrome!"
  False -> do 
   putStrLn "Nope!"
   palindrome'

palindrome'' :: IO ()
palindrome'' = do
 line1 <- getLine
 case ((clean line1) == (reverse (clean line1))) of
  True -> putStrLn $ "It's a palindrome!"
  False -> do 
   putStrLn $ "Nope!"
   palindrome'
  where clean = filter (\x -> x /= ' ' && x /= ',' && x /= '’' && x /= '\'') . (map toLower)
