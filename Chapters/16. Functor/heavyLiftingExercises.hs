module HeavyLiftingExercises where

-- 1
a = fmap (+1) $ read "[1]" :: [Int]

-- 2 
b = (fmap . fmap) (++ "lol") (Just ["Hi,", "Hello"])

-- 3
c x = (*2) ((\x -> x - 2) x)

-- 4
d x = ((return '1' ++) . show) ((\x -> [x, 1..3]) x)
-- 5, copy solution
e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (fmap read (fmap ("123"++) show)) ioi
    in fmap (*3) changed