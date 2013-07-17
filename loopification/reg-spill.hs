{-# LANGUAGE BangPatterns, MagicHash #-}
module Spill where

import GHC.Exts

f :: Float# -> Int# -> Float#
f a 0# = a
f a n  = f (a `timesFloat#` a) (n -# 1#)
