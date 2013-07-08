import qualified Data.Vector as U

main = print $ U.sum $ U.map (\x -> x * x) v
    where v = U.enumFromTo 1 (1000000000 :: Int)
