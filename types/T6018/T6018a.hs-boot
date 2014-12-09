{-# LANGUAGE KindSignatures, TypeFamilies #-}
module T6018a where

type family F a b d = (result :: *) | result -> a d

