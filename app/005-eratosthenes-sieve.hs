-- Still very inefficient
[] \\ ys = []
xs \\ [] = xs
(x:xs) \\ (y:ys) = case compare x y of
    LT -> x : xs \\ (y:ys)
    EQ -> xs \\ ys
    GT -> (x:xs) \\ ys

sieve (p:ps) = p : sieve (ps \\ [p*p, p*p+p..])
eratosthenes n = length $ take n $ 2 : sieve' [3,5..]
