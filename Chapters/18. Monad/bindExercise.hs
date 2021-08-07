module BindExercise where

-- i don't find solution this was a copy
bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ f <$> x
