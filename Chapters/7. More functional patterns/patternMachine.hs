module PatternMachine where


--example 1
isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

--example 2
newtype Username = Username String
newtype AccountNumber = AccountNumber Integer


data User =
     UnregisteredUser
     | RegisteredUser Username AccountNumber


printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name) (AccountNumber acctNum)) =
 putStrLn $ name ++ " " ++ show acctNum
 
myUser = Username "callen"
myAcct = AccountNumber 10456
rUser = RegisteredUser myUser myAcct
-- printerUser rUser
-- printUser UnregisteredUser

--pattern machine and tuples
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = ((snd x, snd y), (fst x, fst y))


f' :: (a, b) -> (c, d) -> ((b, d), (a, c))
f' x y = ((snd x, snd y), (fst x, fst y))

--exercise Fill in the definition of the following function

f1 :: (a, b, c)
 -> (d, e, f)
 -> ((a, d), (c, f))
--f = undefined
f1 (x, y, z) (x', y', z') = ( (x, x') , (z, z'))