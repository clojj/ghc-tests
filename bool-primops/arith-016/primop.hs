{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts

float_text = case (0.0## >=$## 0.0##) of
               1# -> "1"
               0# -> "0"
main = putStrLn (float_text)
