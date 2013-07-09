{-# LANGUAGE BangPatterns, MagicHash #-}
module Spill where

import GHC.Exts

z :: Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int#
z !a 0# _ _ _ _ _ _ _ _ _ _ _ _ _ _ = a
z !a !b !c !d !e !f !g !h !i !j !k !l !m !n !o !p = z (a *# c) (b -# 1#) (d +# 1#) (e +# 1#) (f *# 2#) (g *# 2#) (h +# 2#) (i +# 2#) (j *# 2#) (k +# 3#) (l *# 3#) (m +# 2#) n o p c
