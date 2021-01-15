printSecond :: IO ()
printSecond = do
 putStrLn greeting

greeting = "Yarrrrr"

main :: IO ()
main = do
 putStrLn greeting
 printSecond
 