{-# LANGUAGE TypeFamilies, PolyKinds #-}
{-# LANGUAGE NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures, DatatypeContexts #-}

module T10982 where

{-
type family F (_a :: k) where
    F _t = Int

foo :: a -> a
foo _a = _a
-}

data (Eq _a) => Foo _a = Foo { getFoo :: _a }

type Synonym _a = _a -> _a
-- "Unexpected type '_a' In the type declaration for 'Synonym'"

data A a _b = ACon a a
-- "Unexpected type '_b' In the data declaration for 'A'"

data B _a b = BCon _a _a
-- "Unexpected type '_a' In the data declaration for 'B'"
-}
type family C a (b :: _k) where
    C a b = a -> a
-- "Wildcard '_b' not allowed in a type pattern of family instance for 'C'"
{-
type family D a b where
  D _a b = _a -> _a
-- "Wildcard '_a' not allowed in a type pattern of family instance for 'D'"
-- "Wildcard '_a' not allowed in the declaration for type synonym 'D'" (twice)

data family E a b
data instance E a _b = ECon a a
-- "Wildcard '_b' not allowed in a type pattern of family instance for 'E'"

data family F a b
data instance F _a b = FCon _a _a
-- "Wildcard '_a' not allowed in a type pattern of family instance for 'F'"
-- "Wildcard '_a' not allowed in the definition of data constructor 'FCon'" (twice)
-}
