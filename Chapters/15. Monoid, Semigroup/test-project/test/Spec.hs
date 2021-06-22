import Test.Hspec
import Test.QuickCheck

data Optional a = Nada | Only a deriving (Eq, Show)

type S = String
type B = Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a



main :: IO ()
main = hspec $ do
 describe "Monoids" $ do
  it "has associativity property" $ do
   quickCheck (monoidAssoc :: S -> S -> S -> B)
  it "has left identity" $ do
   quickCheck (monoidLeftIdentity :: String -> Bool)
  it "has right identity" $ do
   quickCheck (monoidRightIdentity :: String -> Bool)
  describe "Exercise: Maybe Another Monoid" $ do
   it "quickCheck test" $ do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)


newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Monoid (First' a) where
 mempty = First' Nada

instance Semigroup (First' a) where
 First' Nada <> (First' (Only x)) = First' (Only x)
 (First' (Only x)) <> First' Nada = First' (Only x)
 (First' (Only x)) <> (First' (Only y)) = First' (Only x)
 First' Nada <> First' Nada = First' Nada
 
firstMappend :: First' a -> First' a -> First' a
firstMappend x y = mappend x y

type FirstMappend = First' String -> First' String -> First' String
 -> Bool
 
type FstId = First' String -> Bool

instance Arbitrary a => Arbitrary (First' a) where 
 arbitrary = do 
  x <- arbitrary
  frequency [ (1, return (First' Nada)), (1, return (First' (Only x))) ]
