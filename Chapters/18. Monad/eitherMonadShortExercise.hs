module EitherMonadShortExercise where


-- you search a lot internet probably you must repeat this exercise
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
 fmap _ (First a)  = First a
 fmap f (Second b) = Second (f b)


instance Applicate (Sum a) where
 pure Second = Second
 (First x) <*> _  = First x
  _ <*> (First x) = First x
  (Second f) <*> (Second x) = Second (f x)
 

instance Monad (Sum a) where
 return = pure
 (First x) >>= _  = First x
 (Second x) >>= f = f x