import Data.List ()

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]

roadStep :: (Path, Path, Int, Int) -> Section -> (Path, Path, Int, Int)
roadStep (pathA, pathB, priceA, priceB) (Section a b c) =
        let forwardPriceToA = priceA + a
            crossPriceToA = priceB + b + c
            forwardPriceToB = priceB + b
            crossPriceToB = priceA + a + c
            newPathToA = if forwardPriceToA <= crossPriceToA
                            then (A,a):pathA
                            else (C,c):(B,b):pathB
            newPathToB = if forwardPriceToB <= crossPriceToB
                            then (B,b):pathB
                            else (C,c):(A,a):pathA
            newSumA    = if forwardPriceToA <= crossPriceToA
                            then priceA + forwardPriceToA
                            else priceA + crossPriceToA
            newSumB    = if forwardPriceToB <= crossPriceToB
                            then priceB + forwardPriceToB
                            else priceB + crossPriceToB
        in  (newPathToA, newPathToB, newSumA, newSumB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
        let (bestAPath, bestBPath, costA, costB) = foldl roadStep ([], [], 0, 0) roadSystem
        in  if costA <= costB
                then reverse bestAPath
                else reverse bestBPath

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main = do
        contents <- getContents
        let threes = groupsOf 3 (map read $ lines contents)
            roadSystem = map (\[a,b,c] -> Section a b c) threes
            path = optimalPath roadSystem
            pathString = concatMap (show . fst) path
            pathPrice = sum $ map snd path
        putStrLn $ "The best path to take is: " ++ pathString
        putStrLn $ "The price is: " ++ show pathPrice
