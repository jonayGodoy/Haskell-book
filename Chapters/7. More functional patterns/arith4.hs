-- arith4.hs
module Arith4 where
-- id :: a -> a
-- id x = x
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- pointfree version of roundTrip
roundTrip2 :: (Show a, Read a) => a -> a
roundTrip2 = read . show

roundTrip3 :: (Show a, Read b) => a -> b
roundTrip3 = read . show

main = do
 print (roundTrip 4)
 print (roundTrip2 4)
 print ((roundTrip 4) :: Int)
 print (id 4)
