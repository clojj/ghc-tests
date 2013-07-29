{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts

{-# NOINLINE foo #-}
foo  :: Double# -> Double# -> Double#
foo a b = (a +## b)

float_text = case (0.0## `foo` 1.2##) of
               0.0##  -> "1"
               _    -> "0"
main = putStrLn (float_text)
