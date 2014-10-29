{-# LANGUAGE MagicHash #-}

module T8326 where

import GHC.Exts

f :: Int# -> Int
f x | isTrue# (x ># 0#) = I# x
    | otherwise         = -(I# x)
