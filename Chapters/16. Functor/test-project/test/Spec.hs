import Test.Hspec
import Test.QuickCheck

-- test functor laws

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f


functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

f :: [Int] -> Bool
f x = functorIdentity x

c = functorCompose (+1) (*2)
li x = c (x :: [Int])

main :: IO ()
main = do
 quickCheck f
 quickCheck li
