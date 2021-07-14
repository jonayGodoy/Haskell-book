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
 quickCheck (\x y -> functorIdentity $ Pair (x::Int) (y::Int))
 quickCheck (\x y -> functorCompose (+1) (*2) $ Pair (x::Int) (y::Int))
 quickCheck (\x y -> functorIdentity $ Two (x::Int) (y::Double))
 quickCheck (\x y -> functorCompose (+1) (*2) $ Two (x::Int) (y::Double))
 quickCheck (\x y z-> functorIdentity $ Three (x::String) (y::Double) (z::Int))
 quickCheck (\x y z-> functorCompose (+1) (*2) $ Three (x::String) (y::Double) (z::Int))
 quickCheck (\x y z-> functorIdentity $ Three' (x::String) (y::Double) (z::Double))
 quickCheck (\x y z-> functorCompose (+1) (*2) $ Three' (x::String) (y::Double) (z::Double))
 quickCheck (\x y z-> functorIdentity $ Three' (x::String) (y::Double) (z::Double))
 quickCheck (\x x' x'' x'''-> functorCompose (+1) (*2) $ 
  Four (x::String) (x'::Double) (x''::Double) (x'''::Int))
 quickCheck (\x x' x'' x'''-> functorIdentity $ 
  Four (x::String) (x'::Double) (x''::Double) (x'''::Int))
 quickCheck (\x x' x'' x'''-> functorCompose (+1) (*2) $ 
  Four' (x::Double) (x'::Double) (x''::Double) (x'''::Int))
 quickCheck (\x x' x'' x'''-> functorIdentity $ 
  Four' (x::Double) (x'::Double) (x''::Double) (x'''::Int))
-- InstancesFuncExercise

-- 1
newtype Identity a = Identity a deriving(Eq, Show)


instance Functor Identity where
 fmap f (Identity a) = Identity (f a) 

-- 2

data Pair a = Pair a a deriving(Eq, Show)

instance Functor Pair where
 fmap f (Pair x y) = Pair (f x) (f y)

-- 3

data Two a b = Two a b deriving(Eq, Show)

instance Functor (Two a) where
 fmap f (Two a b) = Two a (f b)


-- 4

data Three a b c = Three a b c deriving(Eq, Show)

instance Functor (Three a b) where
 fmap f (Three a b c) = Three a b (f c)


-- 5

data Three' a b = Three' a b b deriving(Eq, Show)

instance Functor (Three' a) where
 fmap f (Three' x y z) = Three' x (f y) (f z)

-- 6

data Four a b c d = Four a b c d deriving(Eq, Show)

instance Functor (Four a b c) where
 fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b deriving(Eq, Show)

instance Functor (Four' a) where
 fmap f (Four' x x' x'' y) = Four' x x' x'' (f y)

