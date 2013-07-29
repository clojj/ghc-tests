{-# LANGUAGE MagicHash #-}

module Main where

import Wraps

float_text = case (0# `bar` 3#) of
               True  -> "1"
               False -> "0"
main = putStrLn (float_text)
