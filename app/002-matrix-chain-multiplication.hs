import qualified Data.Vector as V
import Data.Function (fix)

type Input = V.Vector Int
type Problem = (Int, Int)
type Solution = Int

input :: Input
input = V.generate 100 ((* 10) . (+ 1))

mul :: (Int, Int, Int) -> Solution
mul (i, j, k) =
  let x = input V.! i
      y = input V.! j
      z = input V.! k
  in x * y * z

mcm :: (Problem -> Solution) -> Problem -> Solution
mcm f problem@(i, j)
    | i + 1 == j = 0 -- single matrix
    | i + 2 == j = mul (i, i+1, j)
    | otherwise = minimum $ map f' $ subproblems problem
  where
    f' :: (Problem, Problem) -> Solution
    f' (p1@(i, j), p2@(_, j')) =
      let cost1 = f p1
          cost2 = f p2
      in cost1 + cost2 + mul (i, j, j')

subproblems :: Problem -> [(Problem, Problem)]
subproblems (i, j) = do
  x <- [i + 1 .. j - 1]
  [((i, x), (x, j))]

mcm_list :: V.Vector Solution
mcm_list = V.map (mcm mcm_memo) splits

mcm_memo :: Problem -> Solution
mcm_memo problem = mcm_list V.! ix problem

solve = V.last mcm_list

solve' = fix mcm problem
  where
    problem = V.last splits

ix (i,j) =
  let n = length input
      x = (n - j + i + 1)
  in i + (n * (n-1) `div` 2) - ((x-1) * x `div` 2)

splits :: V.Vector Problem
splits = V.concat $ map (window (length input)) [1 .. (length input) - 1]

window :: Int -> Int -> V.Vector Problem
window size window = V.fromList [(n, n + window) | n <- [0 .. size], n + window < size]

main :: IO ()
main = putStrLn $ show solve
