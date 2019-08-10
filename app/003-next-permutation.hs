import Data.Vector as V
import Debug.Trace

input = [1,2,3,4,10,9,8,7,3]

main :: IO ()
main = putStrLn $ show $ nextPermutation $ V.fromList input

nextPermutation item =
  let pIx = pivot item
      sIx = smallest item (pIx) (pIx + 1)
  in V.update item (V.fromList [(pIx, item V.! sIx), (sIx, item V.! pIx)])

pivot ax = go (V.length ax - 1) 0
  where
    go i min
      | i > min && ax V.! i < ax V.! (i - 1) = go (i-1) min
      | otherwise = i - 1

smallest ax tIx maxIx = snd $ V.last $ V.filter ((> ax V.! tIx) . fst) slice
  where
    l = V.length ax
    slice = V.slice maxIx (l - maxIx) $ V.zip ax $ V.iterateN (V.length ax) (+1) 0
