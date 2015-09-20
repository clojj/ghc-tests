{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module T9587 where

type family Arr (repr :: * -> *) (a :: *) (b :: *) = (r :: *) | r -> repr a b

class ESymantics repr where
    int :: Int  -> repr Int
    add :: repr Int  -> repr Int -> repr Int

    lam :: (repr a -> repr b) -> repr (Arr repr a b)
    app :: repr (Arr repr a b) -> repr a -> repr b

te4 = let c3 = lam (\f -> lam (\x -> f `app` (f `app` (f `app` x))))
      in (c3 `app` (lam (\x -> x `add` int 14))) `app` (int 0)
