leapyears :: (Integral x) => x -> [x]
leapyears x = take 20 (iterate (+4) (x + 4 - (x `mod` 4)))
