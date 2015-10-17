{-# LANGUAGE TypeFamilies, UndecidableInstances, ScopedTypeVariables, MultiParamTypeClasses, FunctionalDependencies #-}
module T01832 where

type family F a = r | r -> a

--class G a b r | a b -> r, r -> a b, r -> a b

{-
class G a b r | a b -> r, r a -> b
instance G a b r => G Int (a,b) (Maybe r, a)
instance G Bool a [a]
instance G Char a [a]

class F a b r | a b -> r, r a -> b
instance F a Int r => F Int a (Maybe r)
instance F Bool a [a]
instance F Char a [a]

type family G a b = r | r a -> b where
  G Int  (a,b) = (Maybe (G a b), a)
  G Bool a     = [a]
  G Char a     = [a]

type family F a b = r | r a -> b where
  F Int  a = Maybe (F a Int)
  F Bool a = [a]
  F Char a = [a]

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

  * repeated injectivity conditions (warning only)

       type family Foo a | result -> a, result -> a ...
       type family Foo a b | result -> a b, result -> a b ...
       type family Foo a b | result -> a b, result -> b a ...

  * no repeated variables in injectivity conditions (warning only)

       type family Foo a | result -> a a ...
       type family Foo a b | result b b -> a ...

  * repeated variables on both LHS and RHS of injectivity conditions
    (warning only):

       type family Foo a b c | result a -> a b c ...

  * overlapping conditions (warning only):

       type family Foo a b | result -> a, result -> a b ...
       type family Foo a b | result -> a b, result -> a ...

NOTE: none of the above four corner cases gives any warnings for functional
      dependencies

  * everything above with associated types, eg.:

       class Foo d e where
           type D d e | result d -> e


-}
