module BadMonoid where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
 arbitrary = frequency [ (1, return Fools) , (1, return Twoo) ]


instance Monoid Bull where
 mempty = Fools

instance Semigroup Bull where
 _ <> _ = Fools

instance EqProp Bull where (=-=) = eq

xs = [("b", "w", 1)]


type SSI = (String, String, Int)

trigger :: [SSI]
trigger = undefined

main :: IO ()
main =  do 
 quickBatch (monoid Twoo)
 quickBatch $ applicative trigger