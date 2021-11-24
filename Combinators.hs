module Combinators where

s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)

k :: a -> b -> a
k x y = x

i :: a -> a
i x = x
