{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, KindSignatures, TypeOperators, MultiParamTypeClasses, UndecidableInstances, TemplateHaskell, GADTs, TypeOperators, ScopedTypeVariables, FlexibleContexts, FlexibleInstances, RankNTypes, EmptyDataDecls, FunctionalDependencies #-}
{- LANGUAGE AllowAmbiguousTypes #-}
module T6018 where

type family G (a :: k) where
   G Int  = Bool
   G Bool = Int
   G a    = a

{-
type family Foo a = a

type family G (a :: *) (b :: * -> k2) :: * where
    G a  c = c a
    G Bool c = Int
--    G a    c = a c

-- BUG
type family F a b  = r | r -> a b where
--  F String Maybe c = IO String
  F a     IO      = IO a
  F Char  b       = b Int
-}
--data Proxy a = Proxy

{-
data Foo = A | B
type family Bar (n :: Foo) where
  Bar 'A = Int
  Bar 'B = Char
  Bar a = Double

data Zero
data Succ a

class Add a b ab | a b -> ab, a ab -> b
instance Add Zero b b
instance (Add a b ab) => Add (Succ a) b (Succ ab)

class Show a => C a where
    type F a = r | r -> a
    f :: F a -> a

g :: forall a. C a => a -> String
g _ = show (f undefined :: a)
  where a = f (undefined :: F a) -- :: a

data T a = MkT (F a)
type family F a where
  F (T a) = a
  F (T Int) = Bool

type family Bar a = r | r -> a where
    Bar Int  = Bool
    Bar Bool = Int
    Bar Bool = Char

bar :: Bar a -> Bar a
bar x = x

barapp2 :: Char
barapp2 = bar 'c'

class Manifold' a where
    type Base a = r | r -> a
    project :: a -> Base a
    unproject :: Base a -> a

-- this works
id :: forall a. ( Manifold' a ) => Base a -> Base a
id input = project out
  where
    out :: a
    out = unproject input

-- but this requires injective type families
id' :: forall a. ( Manifold' a ) => Base a -> Base a
id' = project . (unproject :: Base a -> a)





data Base

type family GetParam (p::k) (t::k2) = (r :: k3) | r -> p t

type instance GetParam Base t = t

foo = undefined :: GetParam Base (GetParam Base Int)

-}
{-

type family F a = r | r -> a
type instance F Int = Char


type family F a = r | r -> a where
    F Int = Char

foo :: F a -> Bool
foo = undefined

g = foo 'x'

-}

{-
type family F a = r | r -> a where
    F Char = Bool
    F Bool = Int
    F Int  = Char

idChar :: (F a ~ Bool) => a -> Char
idChar a = a


type family I a = r | r -> a
type instance I Int  = Bool
type instance I Bool = Int

i :: I a -> I a
i x = x

type family Ic a = r | r -> a where
    Ic Int  = Bool
    Ic Bool = Int

ic :: Ic a -> Ic a
ic x = x

type MaybeSyn a = Maybe a

type family B a = r | r -> a

inside :: B a -> a
inside = undefined

outside :: a -> B a
outside = undefined

id :: B a -> B a
id = outside . inside

id2 :: forall a. B a -> B a
id2 = (outside :: a -> B a). inside

-}
{-
type family J a (b :: k) = r | r -> a
type instance J Int Int = Char
type instance J Int b = Char
type instance J Char Char = Int
type instance J Char c = Int

j :: J b Int -> J b Char
j x = x



type family JClosed a (b :: k) = r | r -> a where
    JClosed Int b = Char
    JClosed Char c = Int
    JClosed a d = a

j :: J b Int -> J b Char
j x = x
-}

{-
type family KClosed a = r | r -> a where
    KClosed a = MaybeSyn a

-- should work
jc :: JClosed b Int -> JClosed b Char
jc x = x
-}

{-
import GHC.TypeLits

class Manifold' a where
    type Field a
    type Base  a = r | r -> a
    type Tangent a
    type TangentBundle a
    type Dimension a ::  Nat
    type UsesMetric a :: Symbol
    project :: a -> Base a
    unproject :: Base a -> a
    tangent :: a -> TangentBundle a
    cotangent :: a -> (TangentBundle a -> Field a)

-- this works
id' :: forall a. ( Manifold' a ) => Base a -> Base a
id' input = project out
  where
    out :: a
    out = unproject input

-- but this requires injective type families
id2 :: forall a. ( Manifold' a ) => Base a -> Base a
id2 = project . unproject

$( return [ ClosedTypeFamilyD
            (mkName "Equals")
            [ KindedTV (mkName "a") (VarT (mkName "k"))
            , KindedTV (mkName "b") (VarT (mkName "k")) ]
            ( TyVarSig (KindedTV (mkName "r") (VarT (mkName "k"))))
            (Just $ InjectivityAnn (mkName "r") [mkName "b"])
            [ TySynEqn [ (VarT (mkName "a"))
                       , (VarT (mkName "a")) ]
                       (ConT (mkName "Int"))
            , TySynEqn [ (VarT (mkName "a"))
                       , (VarT (mkName "b")) ]
                       (ConT (mkName "Bool")) ]
            ])
-}
{-
$( return [ DataFamilyD
            (mkName "Equals")
            [ KindedTV (mkName "a") (VarT (mkName "k"))
            , KindedTV (mkName "b") (VarT (mkName "k")) ]
            ( Just StarT)
            ])
$(do { decl <- [d| type family F a b c = result | result -> a b c |]
     ; return decl })

$(do { decl <- [d| type family F a b c = (result :: k) | result -> a b c
                   type instance F Int  Char Bool = Bool
                   type instance F Char Bool Int  = Int
                   type instance F Bool Int  Char = Char |]
     ; return decl })
type family G (a :: k1)
type instance G True = Int
type instance G False = Int



type family Foo a b c d e = r | r -> a c e where
  Foo x y Int a Int = x

$( do FamilyI foo [] <- reify ''Foo
      runIO $ putStrLn $ show foo
      return [] )

f :: InfoQ
f = reify ''Foo
-}



{-
    Baz a = a

type family Bar (_g :: l) :: k where
    Bar a = a

type family Ban (_g :: l) = r where
    Ban a = a
type family Foo (_g :: *) = (r :: k) where
    Foo a = a

type family Fun (_g :: *) = (r :: *) where
    Fun a = a

data N = Z | S N
-}
--type family Foo a = a
{-
type family Baz (a :: k) = r | r -> a

type instance Baz Int = Int

import T6018a
import T6018b

type family G a b c = (result :: *) | result -> a b c
type instance G Int  Char Bool = Bool
type instance G Char Bool Int  = Int
type instance G Bool Int  Char = Int


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




data N = Z | S N

type family P (a :: N) (b :: N) = r | r -> a b where
    P Z n = n
    P (S n) m = S (P n m)
-}
