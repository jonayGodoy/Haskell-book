type S = String
type B = Bool

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

main :: IO ()
main = hspec $ do
 describe "Monoids" $ do
  it "has associativity property" $ do
   quickCheck (monoidAssoc :: S -> S -> S -> B)
