{-# LANGUAGE TypeFamilies, PolyKinds #-}
module T6018 where

{-
type family F a b c | result -> a b c
type instance F Int  Char Bool = Bool
type instance F Char Bool Int  = Int
type instance F Bool Int  Char = Char

type family G a b c | result -> a b
type instance G Int  Char Bool = Bool
type instance G Int  Char Int  = Bool
type instance G Bool Int  Int  = Int

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
type family F a | result -> a where
    F a = a

foo :: F a -> F a
foo = id
-}

{-
type family F a | result -> a where
    F Char = Bool
    F Bool = Int
    F Int  = Char

idChar :: (F a ~ Bool) => a -> Char
idChar a = a
-}
