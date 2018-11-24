module Pachyderm.HList (
    Empty,
    (:.)(..),
    Elem(..),
    Known
) where

data (:.) a b  = a :. b
data Empty

type family Elem needle haystack where
    Elem needle (needle :. rest) = True
    Elem needle (a :. Empty) = False
    Elem needle (a :. rest) = Elem needle rest

type Known x xs = Elem x xs ~ True
