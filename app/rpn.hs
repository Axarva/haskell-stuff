import Data.Char as C ( isDigit )

rpnSolve :: String -> Int
rpnSolve str = read $ head $ foldl (\acc x -> if all C.isDigit x then x : acc else applyFn x (head $ tail acc) (head acc) : drop 2 acc) [] (words str) :: Int
                where applyFn x y z
                        | '+' `elem` x = show $ (read y :: Int) + (read z :: Int)
                        | '-' `elem` x = show $ (read y :: Int) - (read z :: Int)
                        | '*' `elem` x = show $ (read y :: Int) * (read z :: Int)
                        | otherwise    = show $ (read y :: Int) `div` (read z :: Int)
