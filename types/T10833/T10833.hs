{-# LANGUAGE TypeFamilies #-}

module T10833 where

type family F a b c = r | r -> a b c
type instance F Int  Char Bool = Bool
type instance F Char Bool Int  = Int
type instance F Bool Int  Char = Char

f :: (F a b c ~ Bool) => a -> b -> c
f 0 'a' = True
f _ _ = False


type family G a = r | r -> a where
    G Char = Bool
    G Bool = Int
    G Int  = Char

idChar :: (G a ~ Bool) => a -> Char
idChar a = a

fid :: ( F a ~ F b ) => a -> b
fid x = x
