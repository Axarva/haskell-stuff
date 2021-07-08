weirdsum :: (Integral p, Fractional n) => p -> n
weirdsum 0 = 0
weirdsum n = (4 * ( ( (-1) ^ fromIntegral (n + 1) ) / ( (2* fromIntegral n) - 1) )) + weirdsum (n - 1)

main :: IO ()
main = print (weirdsum 100000000000)
