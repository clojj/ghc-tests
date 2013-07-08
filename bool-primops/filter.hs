{-# LANGUAGE BangPatterns, MagicHash #-}
module Filter (
             main
            ) where

import Control.Monad.ST                  (runST)
import Criterion.Config                  (Config, cfgPerformGC,
                                          defaultConfig, ljust)
import Criterion.Main
import Data.Vector.Unboxed.Mutable       (unsafeNew, unsafeSlice, unsafeWrite)
import Data.Vector.Unboxed               as U (Vector, filter, foldM',
                                               fromList, length, unsafeFreeze)
import GHC.Exts                          (Int (I#), (>=$#))
import System.Random                     (RandomGen, mkStdGen, randoms)
import Prelude                    hiding (filter, length)


filterN :: U.Vector Int -> U.Vector Int
filterN vec = runST $ do
  let !size = length vec
  fVec <- unsafeNew size
  let put i x = do
        let !(I# v) = x
            inc     = I# (v >=$# 0#)
        unsafeWrite fVec i x
        return $ i + inc
  fSize <- foldM' put 0 vec
  unsafeFreeze $ unsafeSlice 0 fSize fVec


main :: IO ()
main = return (mkStdGen 1232134332) >>=
       defaultMainWith benchConfig (return ()) . benchmarks


benchmarks :: RandomGen g => g -> [Benchmark]
benchmarks gen =
    let dataSize   = 10 ^ (7 :: Int)
        inputList  = take dataSize . randoms $ gen :: [Int]
        inputVec   = fromList inputList
        isPositive = (> 0)
    in [
       bgroup "Filter"
         [
           bench "Vector" $ whnf (filter  isPositive) inputVec
         , bench "New"    $ whnf (filterN)            inputVec
         ]
      ]


benchConfig :: Config
benchConfig = defaultConfig {
             cfgPerformGC = ljust True
           }