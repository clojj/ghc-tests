module Enum01 where

import Data.Ratio
{-
expr1 :: [Ratio Int]
expr1 = let x = (toEnum ((minBound::Int) + 21))::Ratio Int
        in take 7 [x, x-1 ..]

expr2 :: [Ratio Int]
expr2 = let x = (toEnum ((minBound::Int) + 25))::Ratio Int
        in take 7 [x, x-1 ..]
-}
expr3 :: [Ratio Int]
expr3 = let x = (toEnum (minBound::Int)) :: Ratio Int
        in take 1 [x]
