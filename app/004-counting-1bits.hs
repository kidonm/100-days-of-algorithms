import Data.Bits ((.&.))

-- Biran Kernighan's algorithn
-- https://medium.com/@sanchitbansal10/brian-kernighans-algorithm-9e0ca5989148
kernighan :: Int -> Int
kernighan n = go 0 n
  where
    step n = n .&. (n - 1)
    go ones n
      | n > 0 = go (ones + 1) $ step n
      | otherwise = ones

main :: IO ()
main = putStrLn $ show $ kernighan 255
