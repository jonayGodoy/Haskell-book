module Exercise9MyABS where

myAbs :: Integer -> Integer
myAbs x = if x < 0
           then (x * (-1))
          else
           x