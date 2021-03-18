{-# LANGUAGE NoMonomorphismRestriction #-}
module DoesItCompile where

bigNum = (^) 5 $ 10
wahoo = (^) bigNum $ 10