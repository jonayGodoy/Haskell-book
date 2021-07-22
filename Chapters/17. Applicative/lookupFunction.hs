module LookupFunction where


f x = lookup x 
 [ (3, "hello")
 , (4, "julie")
 , (5, "kbai")]

g y = lookup y 
 [ (7, "sup?")
 , (8, "chris")
 , (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]

m x = lookup x [(4, 10), (8, 13), (1, 9001)]



pureIOConcat = (++) <$> getLine <*> getLine

pureIOTuple = (,) <$> getLine <*> getLine

main :: IO()
main = do
 putStrLn . show $ f 3
 putStrLn . show $ (++) <$> f 3 <*> g 7
 putStrLn . show $ (+) <$> h 5 <*> m 1
 putStrLn . show $ (+) <$> h 5 <*> m 6