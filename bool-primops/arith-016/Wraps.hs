{-# LANGUAGE MagicHash #-}

module Wraps where

import GHC.Exts

{-# NOINLINE foo #-}
foo  :: Double# -> Double# -> Bool
foo a b = tagToEnum# (a >=$## b)

{-# NOINLINE bar #-}
bar  :: Int# -> Int# -> Bool
bar a b = tagToEnum# (a >=$# b)
