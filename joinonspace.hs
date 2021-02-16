import Data.Char (isSpace)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

joinOnSpace :: [[Char]] -> [Char]
joinOnSpace [] = ""
joinOnSpace xs = trim $ head xs ++ " " ++ joinOnSpace (tail xs)
