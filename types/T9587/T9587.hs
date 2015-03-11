{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE AllowAmbiguousTypes #-}

module T9587 where

type family Arr (repr :: * -> *) (a :: *) (b :: *) = (r :: *) | r -> repr a b

class ESymantics repr where
    int :: Int  -> repr Int
    add :: repr Int  -> repr Int -> repr Int

    lam :: (repr a -> repr b) -> repr (Arr repr a b)
    app :: repr (Arr repr a b) -> repr a -> repr b

{-
te4 :: (Arr repr (Arr repr Int Int) (Arr repr Int Int)
        ~
      Arr repr (Arr repr Int Int) (Arr repr Int b),
      ESymantics repr) =>
     repr b
-}
te4 = let c3 = lam (\f -> lam (\x -> f `app` (f `app` (f `app` x))))
      in (c3 `app` (lam (\x -> x `add` int 14))) `app` (int 0)


-- t = lam (\f -> f `app` int 0)

newtype R a = R{unR :: a}
type instance Arr R a b = a -> b

instance ESymantics R where
  int = R
  add (R x) (R y) = R $ x + y
  lam f = R $ unR . f . R
  app (R f) (R x) = R $ f x

tR = unR te4
