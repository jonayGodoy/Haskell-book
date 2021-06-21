import Test.Hspec
import Test.QuickCheck

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
--exercise pending   
--  describe "Exercise: Maybe Another Monoid" $ do
--   it "quickCheck test" $ do
--    quickCheck (monoidAssoc :: FirstMappend)
--    quickCheck (monoidLeftIdentity :: FstId)
--    quickCheck (monoidRightIdentity :: FstId)FstId)
