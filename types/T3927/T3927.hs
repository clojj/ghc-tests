{-# LANGUAGE ScopedTypeVariables, GADTs #-}

module T3972 where

data T a where
  T1 :: T Int
  T2 :: T Bool

-- f1 should be exhaustive
f1 :: T a -> T a -> Bool
f1 T1 T1 = True
f1 T2 T2 = False

-- f2 is exhaustive too, even more obviously
f2 :: T a -> T a -> Bool
f2 T1 (T1 :: T Int) = True
f2 T2 (T2 :: T Bool) = False

foo :: Either t t -> t
foo x | Left  l <- x = l
      | Right r <- x = r

data F a where
  FInt :: F Int
  FBool :: F Bool

class Baz a where
  baz :: F a -> G a
instance Baz Int where
  baz _ = GInt
instance Baz Bool where
  baz _ = GBool

data G a where
  GInt :: G Int
  GBool :: G Bool

bar :: Baz a => F a -> ()
bar a@(FInt) =
  case baz a of
    GInt -> ()
    -- GBool -> ()
bar _ = ()



{-
-- f3's first equation is unreachable
f3 :: T a -> T a -> Bool
f3 T2 T1 = False
f3 _  _  = True
-}
