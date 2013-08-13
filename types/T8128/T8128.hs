{-# LANGUAGE StandaloneDeriving, GADTs, FlexibleInstances #-}

module StandaloneDerivingGADT where

data T a where
  MkT1 :: T Int
  MkT2 :: (Bool -> Bool) -> T Bool

deriving instance Show (T Int)
