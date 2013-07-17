module HeapChecks where

f :: Int -> Float -> Maybe Float
f = \x y -> case x of
              0 -> g (Just y)
              _ -> f (x - 1) y

g :: Maybe a -> Maybe a
g = id
