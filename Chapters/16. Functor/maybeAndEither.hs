module MaybeAndEither where


incMaybe :: Num a => Maybe a -> Maybe a
incMaybe m = fmap (+1) m


showMaybe :: Show a => Maybe a -> Maybe String
showMaybe s = fmap show s


incMaybe'' :: Num a => Maybe a -> Maybe a
incMaybe'' = fmap (+1)


showMaybe'' :: Show a => Maybe a -> Maybe String
showMaybe'' = fmap show


liftedInc :: (Functor f, Num b) => f b -> f b
liftedInc = fmap (+1)


liftedShow :: (Functor f, Show a) => f a -> f String
liftedShow = fmap show


-- Exercise: Possibly

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
 fmap f (LolNope) = LolNope
 fmap f (Yeppers a) = Yeppers (f a)


-- Short Exercise

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
 fmap _ (First a) = First a
 fmap f (Second b) = Second (f b)

