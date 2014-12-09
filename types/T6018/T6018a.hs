{-# LANGUAGE TypeFamilies, PolyKinds, DataKinds, KindSignatures, TypeOperators, MultiParamTypeClasses #-}

module T6018a where

import T6018

type family F a b c = (result :: *) | result -> a c
