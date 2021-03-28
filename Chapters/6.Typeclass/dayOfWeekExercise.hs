{-# LANGUAGE NoMonomorphismRestriction #-}
module DayOfWeekExercise where

data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun

instance Eq DayOfWeek where
 (==) Mon Mon   = True
 (==) Tue Tue   = True
 (==) Weds Weds = True
 (==) Thu Thu   = True
 (==) Fri Fri   = True
 (==) Sat Sat   = True
 (==) Sun Sun   = True
 (==) _ _       = False
 
instance Show DayOfWeek where
  show Mon  = "Monday"
  show Tue  = "Tuesday"
  show Weds = "Wednesday"
  show Thu  = "Thursday"
  show Fri  = "Fri"
  show Sat  = "Saturday"
  show Sun  = "Sunday"
 
data Date = Date DayOfWeek Int

instance Eq Date where
 (==) (Date weekday dayOfMonth)
      (Date weekday' dayOfMonth') =
     weekday == weekday'
  && dayOfMonth == dayOfMonth'
  
instance Show Date where
 show (Date weekday dayOfMonth) = show weekday ++ " " ++ show dayOfMonth 
