import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

-- test functor laws

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f


functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

f :: [Int] -> Bool
f x = functorIdentity x

c = functorCompose (+1) (*2)
li x = c (x :: [Int])

type IntToInt = Fun Int Int
type IntFC = [Int] -> IntToInt -> IntToInt -> Bool

main :: IO ()
main = do
 quickCheck f
 quickCheck li
 quickCheck (functorCompose' :: IntFC)
 quickCheck (\x -> functorIdentity $ Identity (x::Int))
 quickCheck (\x -> functorCompose (+1) (*2) $ Identity (x::Int))

-- InstancesFuncExercise

-- 1
newtype Identity a = Identity a deriving(Eq, Show)


instance Functor Identity where
 fmap f (Identity a) = Identity (f a)



