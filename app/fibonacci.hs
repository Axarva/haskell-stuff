fiboCalc :: (Eq a, Num a) => a -> a -> a -> a
fiboCalc a b c
    | c == 0 = a + b
    | otherwise = fiboCalc b (a + b) (c - 1)

fibonacci :: Integer -> Integer
fibonacci n
    | n == 0  = error "Index starts at 1"
    | n == 1  = 0
    | n == 2  = 1
    | otherwise = fiboCalc 0 1 (n - 3)

