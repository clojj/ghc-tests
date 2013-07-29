{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts

{-# NOINLINE bar #-}
bar  :: Int# -> Int# -> Bool
bar a b = tagToEnum# (a >=$# b)

float_text = case (0# `bar` 3#) of
               True  -> "1"
               False -> "0"
main = putStrLn (float_text)
