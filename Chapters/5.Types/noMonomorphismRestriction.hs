{-# LANGUAGE NoMonomorphismRestriction #-}

module UnDetemineTheType where
-- for default haskell has polymorphic restriccion in files

example = 1
-- example :: Num p => p
-- example is Num when we cancel restriction because haskell choose generic type