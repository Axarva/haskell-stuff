factors :: Integral a => a -> [a]
factors x = x : [y | y <- [1..(x`div`2)], x `mod` y == 0] 

composites :: (Integral a, Num b) => a -> [b]
composites n = map (fromIntegral . head) $ filter (\x -> length x > 2) $ map factors [4..n]
