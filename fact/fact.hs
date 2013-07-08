{-# LANGUAGE MagicHash #-}
module Fact where

import GHC.Prim

fact :: Int# -> Int# -> Int#
fact 0# a = a
fact n  a = fact (n -# 1#) (a *# n)
