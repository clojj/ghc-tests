{-# LANGUAGE MagicHash #-}

module Main where

import Wraps

float_text = case (0.0## `foo` 1.2##) of
               True  -> "1"
               False -> "0"
main = putStrLn (float_text)