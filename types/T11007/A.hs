{-# LANGUAGE TypeFamilies #-}

module A where

import B

type family Id x = r | r -> x where
  Id a = a
