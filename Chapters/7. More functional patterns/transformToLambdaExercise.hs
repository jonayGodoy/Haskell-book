module TransformToLambdaExercise where

--1 rewrite the f function in the where clause
addOneIfOdd1 n = case odd n of
    True -> f n
    False -> n
    where f n = n + 1


addOneIfOdd2 n = case odd n of
    True -> (\n -> n + 1) n
    False -> n


--2 rewrite the following to use anonymous lambda syntax:
addFive1 x y = (if x > y then x else x) +5


addFive2 = \x -> \y -> (if x > y then x else x) +5

--2 rewrite the following so that it doesn't use anonymous lamda syntax
mflip1 f = \x -> \y -> f y x

mflip2 f x y = f y x






