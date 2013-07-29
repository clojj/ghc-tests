{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts

float_text = case (tagToEnum# (0.0## >=$## 0.0##) :: Bool) of
               True  -> "1"
               False -> "0"
main = putStrLn (float_text)
