module ListComprehensions where

mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]

--1 exercise
mySqrAndMyCube = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]

howMuch xs = length  xs
