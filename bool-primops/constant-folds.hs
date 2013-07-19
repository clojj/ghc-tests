module ConstantFolds where

{-
-}
quotInteger :: Integer
quotInteger = 100063 `quot` 156

remInteger :: Integer
remInteger = 100064 `rem` 156

quotRemInteger :: (Integer, Integer)
quotRemInteger = 100059 `quotRem` 123

divModInteger :: (Integer, Integer)
divModInteger = 100060 `divMod` 456
{-
-}