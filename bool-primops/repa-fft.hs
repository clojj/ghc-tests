{-# LANGUAGE MagicHash, BangPatterns #-}
module Main (
    main,
    loadData,
    transformPrimops,
    transformP
 ) where

import Control.Monad
import Data.Word
import GHC.Exts

import Criterion.Main
import Data.Array.Repa                          as R
import Data.Array.Repa.Algorithms.FFT
import Data.Array.Repa.Algorithms.Complex
import Data.Array.Repa.Algorithms.DFT.Center
import qualified Data.Array.Repa.Repr.Unboxed   as U
import Data.Array.Repa.IO.BMP
import System.CPUTime
import Text.Printf

{-main :: IO ()
main = do
  putStrLn "Starting..."
  lena <- loadData "lena.bmp"
  _    <- time $ transformP 5 lena

  putStrLn "Timing normal transform..."
  time1 <- liftM sum (replicateM 10 (time $ transformP 5 lena))
  let timeAvg1 = time1 / 10.0
  printf "Average normal time: %0.3f sec\n" timeAvg1

  putStrLn "Timing new primops..."
  time2 <- liftM sum (replicateM 10 (time $ transformPrimops 5 lena))
  let timeAvg2 = time2 / 10.0
  printf "Average primops time: %0.3f sec\n" timeAvg2

  printf "Speedup is: %0.3f percent\n" ((timeAvg1 - timeAvg2) / timeAvg1 * 100.0)
  return ()

time :: IO t -> IO Double
time a = do
    start <- getCPUTime
    v     <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return diff
-}

main :: IO ()
main = loadData "../data/lena.bmp" >>= defaultMain . benchmarks


benchmarks :: Array U DIM2 Word8 -> [Benchmark]
benchmarks image = [
    bench "normal FFT"      $ whnfIO (transformP       5 image)
  , bench "new primops FFT" $ whnfIO (transformPrimops 5 image)
  ]


loadData :: String -> IO (Array U DIM2 Word8)
loadData fileIn = do
  arrRGB <- liftM (either (\e -> error $ show e) id) $ readImageFromBMP fileIn
  let (arrRed, _, _) = U.unzip3 arrRGB
  return arrRed


transformPrimops :: Monad m
                 => Int
                 -> Array U DIM2 Word8
                 -> m (Array U DIM2 Word8)
transformPrimops (I# cutoff) arrReal = do
        let arrComplex = R.map (\r -> (fromIntegral r, 0)) arrReal

        -- Do the 2d transform.
        arrCentered <- computeUnboxedP $ center2d arrComplex
        arrFreq     <- fft2dP Forward arrCentered

        -- Zap out the low frequency components.
        let _ :. height :. width = extent arrFreq
        let !(I# centerX)        = width  `div` 2
        let !(I# centerY)        = height `div` 2
        let {-# INLINE highpass #-}
            highpass get ix@(_ :. (I# y) :. (I# x)) =
                  case  (x >$# (centerX +# cutoff)) +#
                        (x <$# (centerX -# cutoff)) +#
                        (y >$# (centerY +# cutoff)) +#
                        (y <$# (centerY -# cutoff)) of
                    0# -> 0
                    _  -> get ix

        arrFilt        <- computeUnboxedP $ traverse arrFreq id highpass

        -- Do the inverse transform to get back to image space.
        arrInv        <- fft2dP Inverse arrFilt

        -- Get the magnitude of the transformed array,
        computeUnboxedP $ R.map (truncate . mag) arrInv


transformP :: Monad m
           => Int
           -> Array U DIM2 Word8
           -> m (Array U DIM2 Word8)
transformP cutoff arrReal = do
        let arrComplex = R.map (\r -> (fromIntegral r, 0)) arrReal

        -- Do the 2d transform.
        arrCentered <- computeUnboxedP $ center2d arrComplex
        arrFreq     <- fft2dP Forward arrCentered

        -- Zap out the low frequency components.
        let _ :. height :. width = extent arrFreq
        let centerX                 = width  `div` 2
        let centerY                 = height `div` 2
        let {-# INLINE highpass #-}
            highpass get ix@(_ :. y :. x)
                  |   x > centerX + cutoff
                   || x < centerX - cutoff
                   || y > centerY + cutoff
                   || y < centerY - cutoff
                    = get ix
                  | otherwise = 0

        arrFilt        <- computeUnboxedP $ traverse arrFreq id highpass

        -- Do the inverse transform to get back to image space.
        arrInv        <- fft2dP Inverse arrFilt

        -- Get the magnitude of the transformed array,
        computeUnboxedP $ R.map (truncate . mag) arrInv
