{-# LANGUAGE NoMonomorphismRestriction #-}
module DoesItCompile where

-- 1 fix it
bigNum = (^) 5 $ 10
wahoo = (^) bigNum $ 10

-- 2 i didn't understand
x = print
y = print "woohoo!"
z = x "hello world"

-- 3 fix it
a = (+)
b = 5
c = a b 10
d = a c 200

-- 4 fix it but i didn't understand
a1 = 12 + b1
b1 = 10000 * c1
c1 = 1