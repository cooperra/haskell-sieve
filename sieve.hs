primes = sieve [2..]

sieve :: Integral a => [a] -> [a]
sieve [] = []
sieve (x:xs) = x:(sieve $ takeNonMultiples xs)
  where takeNonMultiples = helper x
        helper _ [] = []
        helper nextMultiple xs = half1 ++ (helper (n+x) $ dropWhile (==n) half2)
          where (half1, half2) = span (<n) xs
                n = nextMultiple

sieve2 :: Integral a => [a] -> [a]
sieve2 [] = []
sieve2 (x:xs) = x:(sieve2 $ takeNonMultiples xs)
  where takeNonMultiples = helper x
        helper _ [] = []
        helper nextMultiple (y:ys)
          | y <  n = y:(helper  n       ys)
          | y == n =    helper  n       ys
          | y >  n =    helper (n+x) (y:ys)
          where n = nextMultiple

-- main = putStrLn . show $ take 100 primes
main = do
  putStrLn "How many? "
  count <- getLine
  putStrLn . show $ take (read count) primes
