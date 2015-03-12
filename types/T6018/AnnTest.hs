{-# LANGUAGE TypeFamilies #-}

module AnnTest where

type family Baz a = r | r -> a where
    Baz Int = Int
