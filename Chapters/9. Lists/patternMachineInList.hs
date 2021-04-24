module PatternMachineInList where

myHead (x:_) = x

myTail (_:xs) = xs

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x