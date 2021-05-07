module DatabaseProcessingFoldExercises where

import Data.Time


data DatabaseItem = DbString String
                    | DbNumber Integer
                    | DbDate UTCTime
                    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
   [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
            (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbDate (UTCTime
             (fromGregorian 1921 5 1)
             (secondsToDiffTime 34123))
    , DbNumber 101
    ]


-- 1. Write a function that filters for DbDate values and returns 
--a list of the UTCTime values inside them.

isDbDate :: DatabaseItem -> Bool
isDbDate (DbDate _) = True
isDbDate _ = False

getDbDate :: DatabaseItem -> UTCTime
getDbDate (DbDate utcTime) = utcTime


filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = foldr (\x y -> if isDbDate x then [getDbDate x] ++ y else y) [] xs

--2. Write a function that filters for DbNumber values and returns
--a list of the Integer values inside them.

isDbNumber :: DatabaseItem -> Bool
isDbNumber (DbNumber _) = True;
isDbNumber _ = False

getDbNumber :: DatabaseItem -> Integer
getDbNumber (DbNumber integer) = integer

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = foldr (\x y -> if isDbNumber x then [getDbNumber x] ++ y else y) [] xs

--3. Write a function that gets the most recent date.



mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = foldr 
              (\x y -> if isDbDate x then chooseTheMostRecent (getDbDate x) y else y)
              (UTCTime (fromGregorian 1700 5 1)(secondsToDiffTime 34123))
              xs
              where chooseTheMostRecent :: UTCTime -> UTCTime -> UTCTime
                    chooseTheMostRecent x y = if x > y then x else y 

--4. Write a function that sums all of the DbNumber values

sumDb :: [DatabaseItem] -> Integer
sumDb xs = foldr 
              (\x y -> if isDbNumber x then (getDbNumber x) + y else y)
              0
              xs
  
-- 5. Write a function that gets the average of the DbNumber values.

avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral (sumDb xs) / fromIntegral (length (filterDbNumber xs))
