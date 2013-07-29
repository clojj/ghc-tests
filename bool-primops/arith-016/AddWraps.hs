{-# LANGUAGE MagicHash #-}
module AddWraps where

import GHC.Exts

{-
{-# NOINLINE foo #-}
foo  :: Int# -> Int# -> Int
foo a b = I# (a +# b)
-}
{-# NOINLINE foo #-}
foo  :: Double# -> Double# -> Double#
foo a b = (a +## b)
{-
{-# NOINLINE foo #-}
foo  :: Float# -> Float# -> Float
foo a b = F# (a `plusFloat#` b)
-}
