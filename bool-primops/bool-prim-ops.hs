{-# LANGUAGE MagicHash, BangPatterns #-}
module BoolPrimOps where

import GHC.Exts

{-fw :: Int -> Int -> Int -> Int -> Int
fw (I# x) (I# y) (I# aWidth) (I# aHeight) =
    f x y aWidth aHeight

gw :: Int -> Int -> Int -> Int -> Int
gw (I# x) (I# y) (I# aWidth) (I# aHeight) =
    g x y aWidth aHeight
-}

{-# INLINE f #-}
f :: Int# -> Int# -> Int# -> Int# -> Int
f x y width height =
    case (x <$# 0#) `orI#` (x >=$# width) `orI#` (y <$# 0#) `orI#` (y >=$# height) of
      1# -> 1
      0# -> 0
{-

f' :: Int# -> Int# -> Int# -> Int# -> Int
f' x y width height =
    case (x <# 0#) || (x >=# width) || (y <# 0#) || (y >=# height) of
      True -> 1
      False -> 0

{-# INLINE g #-}
g :: Int# -> Int# -> Int# -> Int# -> Int
g x y aWidth aHeight =
    case x ># aWidth || y ># aHeight of
      False -> 0
      True  -> 1
-}