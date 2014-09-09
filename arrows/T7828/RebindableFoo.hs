{-# LANGUAGE RebindableSyntax #-}

module RebindableFoo where

import Prelude (Maybe(..), Int, ($), (+), fromInteger, undefined)

class Foo a where

foo :: Foo a => Maybe a -> Maybe a
foo y = do
  x <- y
  return $ x

(>>=) :: Foo a => Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>= _ = Nothing
Just x >>= f = f x

return :: a -> Maybe a
return x = Just x

fail = undefined
