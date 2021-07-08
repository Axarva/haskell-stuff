multisum :: Integral a => a -> a
multisum x = sum [ l | l <- [1..x], l `mod` 3 == 0 || l `mod` 5 == 0]


