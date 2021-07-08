factors :: Integral a => a -> [a]
factors x = [n | n <- [2..x], x `mod` n == 0, n /= x]

primes :: [Integer]
primes = take 20 [x | x <- [2,3..], null (factors x)]

