module ArtFullDodgyHOFExercise where

dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

--dodgy 1 0 -> 1