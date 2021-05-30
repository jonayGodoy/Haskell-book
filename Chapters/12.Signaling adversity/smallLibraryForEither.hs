module SmallLibraryForEither where


lefts' :: [Either a b] -> [a]
lefts' xs = foldr (\x y -> if isLeft x then (fromLeft x) : y else y) [] xs
 where isLeft (Left x) = True
       isLeft (Right _) = False
       fromLeft (Left x) = x



rights' :: [Either a b] -> [b]
rights' xs = foldr (\x y -> if isRight x then (fromRight x) : y else y) [] xs
 where isRight (Right _) = True
       isRight (Left x) = False
       fromRight (Right x) = x