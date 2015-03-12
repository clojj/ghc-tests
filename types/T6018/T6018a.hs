{-# LANGUAGE TypeFamilies, PolyKinds, DataKinds, KindSignatures, TypeOperators, MultiParamTypeClasses #-}

module T6018a where

type family F a b c = (result :: *) | result -> a c

{-# DEPRECATED foo
               "Lorem sadd asd ad  d ad  sa d as d as d a  da  sd as d a sd sa  d asds ipsum" #-}
foo x = x
