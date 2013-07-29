{-# LANGUAGE MagicHash #-}

module Main where

import AddWraps

float_text = case (0.0## `foo` 1.0##) of
               0.0## -> "1"
               _     -> "0"
main = putStrLn (float_text)
