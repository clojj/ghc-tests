{-# LANGUAGE TypeFamilies, PolyKinds, DataKinds, KindSignatures, TypeOperators, MultiParamTypeClasses #-}
{- LANGUAGE AllowAmbiguousTypes #-}
module T6018 where

import T6018a


--type family F a b c = (result :: *) | result -> a b c
--type instance F Int  Char Bool = Bool
--type instance F Char Bool Int  = Int

type instance F Bool Int  Char = Int

{-
type family D a b c = (result :: *) | result -> a b c
type instance D Char Bool Int  = Bool
type instance D Char Bool Int  = Int
type instance D Bool Int  Char = Int

-}
{-

type family Fc a b c = a | a -> a b where
    Fc Int  Char Bool = Bool
    Fc Char Bool Int  = Int
    Fc Bool Int  Char = Char

class Fcl a b c where
    type Foo a b = r | r -> a b

type family G a b c = a | a -> a b c
type instance G Int  Char Bool = Bool
type instance G Int  Char Int  = Bool
type instance G Bool Int  Int  = Int

type family Gc a b c = a | a -> a b c where
    Gc Int  Char Bool = Bool
    Gc Int  Char Int  = Bool
    Gc Bool Int  Int  = Int

type family H a b c | result a -> b c, result b -> a c, result c -> a b
type instance H Int  Char   Double = Int
type instance H Bool Double Char   = Int

type family J a b c | result a -> b c, result b -> a c
type instance J Int  Char   Double = Int
type instance J Bool Double Double = Int

type family F a b c | result -> a b c where
     F Int  Char Bool = Bool
     F Char Bool Int  = Int
     F Bool Int  Char = Char

type family G a b c | result -> a b where
     G Int  Char Bool = Bool
     G Int  Char Int  = Bool
     G Bool Int  Int  = Int

type family H a b c | result a -> b c, result b -> a c, result c -> a b where
     H Int  Char   Double = Int
     H Bool Double Char   = Int

type family J a b c | result a -> b c, result b -> a c where
     J Int  Char   Double = Int
     J Bool Double Double = Int

Corner cases to test:

  * type variable repeated in the type family head (done already, I think)

       type family Foo a a | result -> a ...

  * kind variable introduced in TF head and then used in injectivity condition

       type family Foo (a :: k) | result -> k ...

  * not in-scope type variable used in injectivity condition

       type family Foo a | result -> b ...

  * repeated injectivity conditions (warning only)

       type family Foo a | result -> a, result -> a ...
       type family Foo a b | result -> a b, result -> a b ...
       type family Foo a b | result -> a b, result -> b a ...

  * no repeated variables in injectivity conditions (warning only)

       type family Foo a | result -> a a ...
       type family Foo a b | result b b -> a ...

  * no repeated variables on both LHS and RHS of injectivity conditions
    (warning only):

       type family Foo a b c | result a -> a b c ...

  * no overlapping conditions (warning only):

       type family Foo a b | result -> a, result -> a b ...
       type family Foo a b | result -> a b, result -> a ...

  * everything above with associated types, eg.:

       class Foo d e where
           type D d e | result d -> e

    Does that even make sense? We would need to check injectivity when
    new instances are added.

-}

{-
type family F a b c -- | result -> a b c
type instance F Int  Char Bool = Bool
type instance F Char Bool Int  = Int
type instance F Bool Int  Char = Char

f :: (F a b c ~ Bool) => a -> b -> c
f 0 'a' = True
f _ _ = False
-}

{-
type family F a = r | r -> a where
    F a = a

type family G a = r | r -> a where
    G Int = Int
    G Bool = Bool
    G a = a
-}

{-
type family Map (f :: k -> l) (ks :: [k]) :: [l] where
    Map f '[] = '[]
    Map f (x ': xs) = f x ': Map f xs

data Proxy a = Proxy

foo :: Proxy (Map Maybe [Int, Char])
foo = undefined


Map f [a, b] ~ [Maybe Int, Maybe Char]


type family K a = r | r -> a where
    K Int = Bool
    K Bool = Int
    K a = a
-}
--    F a = a
--    F Bool = Bool
--    F a = a
{-

foo :: F a -> F a
foo = id

bar :: Int -> Int
bar = foo




type family F a | result -> a where
    F Char = Bool
    F Bool = Int
    F Int  = Char

idChar :: (F a ~ Bool) => a -> Char
idChar a = a

data N = Z | S N

type family P (a :: N) (b :: N) = r | r -> a b where
    P Z n = n
    P (S n) m = S (P n m)
-}
