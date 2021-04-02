module DayOfWeekSecondVersion where

data DayOfWeek =
 Mon | Tue | Weds | Thu | Fri | Sat | Sun
 deriving (Show, Eq)
 
 
-- But if we wanted to express that Friday is always the best day, we can write our own Ord instance

instance Ord DayOfWeek where
 compare Fri Fri = EQ
 compare Fri _ = GT
 compare _ Fri = LT
 compare _ _ = EQ