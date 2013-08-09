module Main(main) where

import Enum01

main :: IO ()
main = do
  print expr3
{-
  print expr2
  let x = (toEnum ((minBound::Int) + 1))::Ratio Int
  print (take 7 [x, x-1 ..])
  let x1 = (toEnum ((minBound::Int) + 5))::Ratio Int
  print (take 7 [x1, x1-1 ..])
-}
