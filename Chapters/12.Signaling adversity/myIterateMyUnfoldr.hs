module MyIterateMyUnfoldr where

import Data.Maybe

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = (fst (fromJust (f x))) : myUnfoldr f (snd (fromJust (f x)))

myUnfoldr2 :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr2 f x = go f x (f x)
 where go _ _ Nothing = []
       go f _ (Just (x, y)) = x : myUnfoldr f y


betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr2 (\x' -> Just (x', (f x'))) x