-- Copied from: https://rosettacode.org/wiki/Towers_of_Hanoi#Haskell
hanoi :: Integer -> a -> a -> a -> [(a, a)]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

main :: IO ()
main = putStrLn $ show $ hanoi 10 'a' 'b' 'c'

