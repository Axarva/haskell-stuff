containsUppercase :: [Char] -> Bool
-- The only function that I wrote.
containsUppercase xs = or ([ z |  z <- [True, False], y <- xs, z == elem y ['A'..'Z']])

-- Courtesy of my IDE, VSCodium. I should stop coding and let my IDE handle everything,
-- I don't even know what this code is. It literally just corrected me.

containsUppercase2 :: [Char] -> Bool
containsUppercase2 = any (\x -> x `elem` ['A' .. 'Z'])

containsUppercase3 :: [Char] -> Bool
containsUppercase3 = foldr (\ x -> (||) (x `elem` ['A' .. 'Z'])) False
