module SmallLibraryForEither where


lefts' :: [Either a b] -> [a]
lefts' xs = foldr (\x y -> if isLeft x then (fromLeft x) : y else y) [] xs

isLeft :: Either a b -> Bool
isLeft (Left x) = True
isLeft (Right _) = False

fromLeft :: Either a b -> a
fromLeft (Left x) = x

rights' :: [Either a b] -> [b]
rights' xs = foldr (\x y -> if isRight x then (fromRight x) : y else y) [] xs

fromRight :: Either a b -> b
fromRight (Right x) = x

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left x) = False

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs =
 foldr 
  (\x y  -> 
         if isLeft x 
          then (((fromLeft x) : fst y), snd y)
           else (fst y, ((fromRight x) : snd y))
   ) 
  ([],[])
  xs


eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left a) = Nothing
eitherMaybe' f (Right b) = Just (f b)


either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ f (Right b) = f b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (\x -> Nothing) (\x -> Just (f x)) x