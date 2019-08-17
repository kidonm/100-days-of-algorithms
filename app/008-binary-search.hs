import Debug.Trace
import Data.Vector as V

binarySearch :: Ord a => V.Vector a -> a -> Int
binarySearch v a = go 0 $ V.length v - 1
  where
    go low high
        | low > high = (-1)
        | midValue == a = mid
        | midValue < a = go (mid + 1) high
        | otherwise = go low $ mid - 1
      where
        mid = (low + high) `div` 2
        midValue = v V.! mid

main :: IO ()
main = putStr $ show $ binarySearch input elem
  where
    len = 1000000
    elem = len - 1
    input = V.generate len id
