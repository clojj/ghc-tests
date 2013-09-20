{-# LANGUAGE MagicHash #-}
module A where

import GHC.Prim
--import GHC.Base
import GHC.Exts hiding (isTrue#)
--import GHC.PrimWrappers

{-
f :: Int# -> Int
f x | isTrue# (x ># 0#) = I# x
    | otherwise            = -(I# x)

f :: Int# -> Int
f x = case (x ># 0#) of
        1# -> I# x
        _  -> -(I# x)
-}

f,g :: Int# -> Int
f x | tagToEnum# (x >$# 0#) = I# x
    | otherwise            = I# (0# -# x)

g x | isTrue# (x >$# 0#) = I# x
    | otherwise         = I# (0# -#  x)

isTrue# 0# = True
isTrue# _  = False
