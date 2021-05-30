module OnlyNatural where



data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + (natToInteger x)

integerToNat :: Integer -> Maybe Nat
integerToNat x
 | x == 0 = Just Zero
 | x < 0 = Nothing
 | otherwise = Just $ extractNat x
   where extractNat 0 = Zero
         extractNat x = Succ $ extractNat (x-1)