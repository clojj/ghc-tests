module InjectivityCondsCounter where

import Data.List


powset :: Int -> [[Int]]
powset 0 = [[]]
powset n = map (n:) ps ++ ps
    where ps = powset (n - 1)


injConds :: Int -> [([Int],[Int])]
injConds n = [ (l, r) | l <- ps, r <- ps, not (null r), canMerge l r ]
    where ps = powset n
          canMerge (x:xs) (y:ys) | x == y = False
                                 | x < y  = canMerge (x:xs) ys
                                 | x > y  = canMerge xs (y:ys)
          canMerge _      _      = True


pprInjConds :: [([Int],[Int])] -> [String]
pprInjConds cnds = go cnds
    where n = maximum $ concatMap snd cnds
          letters = ['a'..]
          name m  = letters !! (n - m)
          go [] = []
          go ((lhs,rhs):xs)  = injCond : go xs
              where namedLHS = map name lhs
                    namedRHS = map name rhs
                    injCond  = "result " ++ intersperse ' ' namedLHS
                                         ++ case namedLHS of {[]->[]; _ -> " "}
                                         ++ "-> "
                                         ++ intersperse ' ' namedRHS
