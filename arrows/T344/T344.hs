{-# LANGUAGE Arrows, ExistentialQuantification #-}
{-# OPTIONS -dcore-lint #-}
module T344 where

class Foo a where foo :: a -> ()
data Bar = forall a. Foo a => Bar a

get :: Bar -> ()
get = proc x -> case x of Bar a -> id -< foo a
