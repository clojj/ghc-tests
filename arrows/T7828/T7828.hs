{-# LANGUAGE GADTs, Arrows, NullaryTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
module T7828 where

import Prelude          (Either(..), flip, Int, return)

import Control.Category (Category)
import Control.Arrow    (Arrow)

test :: Foo a => R a a
test = proc n -> returnA -< n
--test =
--    (>>>)
--      (arr (\ (n_apd) -> n_apd))
--      ((>>>)
--         (arr (\ (ds_dst) -> ds_dst))
--         (returnA)
--         )

instance Category R where
instance Arrow R where

class Foo a where

data R a b where
  Id       :: R a a
  Comp     :: R b c -> R a b -> R a c
  Arr      :: (a -> b) -> R a b
  Split    :: R b c -> R b' c' -> R (b,b') (c,c')

infixr 1 >>>
infixr 3 ***

arr :: (Foo a)
    => (a -> b) -> R a b
arr = Arr

first :: R b c -> R (b, d) (c, d)
first = (*** Id)

(***) :: R b c -> R b' c' -> R (b,b') (c,c')
(***) = Split

(>>>) :: R a b -> R b c -> R a c
(>>>) = flip Comp

returnA :: R a a
returnA = Id
