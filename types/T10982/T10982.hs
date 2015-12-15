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
