{-# LANGUAGE MagicHash #-}

module Main where

import GHC.Exts

float_text = case (0.0## >=## 0.0##) of
               True  -> "1"
               False -> "0"
main = putStrLn (float_text)