module GimmePerson where

import System.IO

type Name = String

type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
 | name /= "" && age > 0 = Right $ Person name age
 | name == "" = Left NameEmpty
 | not (age > 0) = Left AgeTooLow
 | otherwise = Left $ PersonInvalidUnknown 
      $ "Name was: " ++ show name ++ " Age was: " ++ show age


gimmePerson :: IO ()
gimmePerson = do
 hSetBuffering stdout NoBuffering
 putStr "Please introduce name:"
 name <- getLine
 putStr "Please introduce age:"
 rawAge <- getLine
 putStrLn $ formatEither $ mkPerson name (read rawAge :: Integer)
 where formatEither :: Either PersonInvalid Person -> String
       formatEither (Left x) = show x
       formatEither (Right x) = show x
 