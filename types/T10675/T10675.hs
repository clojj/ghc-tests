module T10675 where

type family G a b c = r | r a -> b where
     G Int (a,b) Char = (Maybe (H a b), a)
     G Int (a,a) ()   = (Maybe a, a)

class H a b r | a b -> r, r a -> b
instance H Bool Double Bool

class G a b c r | a b c -> r, r a -> b
instance H a b r => G Int (a,b) ()   (Maybe r, a)
instance            G Int (a,a) Char (Maybe a, a)

foo :: G a b c r => (a,b,c,r)
foo = undefined

-- (1): instance G Int (Bool, Bool)   Char (Maybe Bool, Bool)
x = foo :: (Int, (Bool, Bool), Char, (Maybe Bool, Bool))

-- (2): instance G Int (Bool, Double) ()   (Maybe Bool, Bool)
y = foo :: (Int, (Bool, Double), (), (Maybe Bool, Bool))
