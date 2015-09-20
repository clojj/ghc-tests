module T01832 where

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

type family F a b = r | r -> b
type instance F c Int = (c, c)
type instance F d Bool = (d, [d])

type family G where
  G = [G]


-- F G Int ~ (G, G) ~ (G, [G]) ~ F G Bool

class Show a => C a where
    type F a = r | r -> a
    f :: F a -> a

g :: forall a. C a => a -> String
g _ = show (f undefined :: a)
  where a = f (undefined :: F a) -- :: a

inside :: B a -> a
inside = undefined

outside :: a -> B a
outside = undefined

id :: B a -> B a
id = outside . inside

id2 :: forall a. B a -> B a
id2 = (outside :: a -> B a). inside

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
