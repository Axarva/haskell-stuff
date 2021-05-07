zipWith' :: (t1 -> t2 -> a) -> [t1] -> [t2] -> [a]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y:zipWith' f xs ys

flip' :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3
flip' f x y = f y x

applyThrice :: (t -> t) -> t -> t
applyThrice f x = f $ applyTwice f x

applyTwice :: (t -> t) -> t -> t
applyTwice f x = f $ f x

map' :: (t -> a) -> [t] -> [a]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

chain :: Integral a => a -> [a]
chain x
    | x == 1 = [1]
    | odd x  = x: chain (x * 3 + 1)
    | even x = x: chain (x `div` 2)

numLongChains :: Int
-- Readable definition.
numLongChains = 
    let chains = map chain [1..100]
        isLong xs = length xs > 15
    in length (filter isLong chains)

-- Easier definition.
-- numLongChains = length $ filter (>15) $ map chain [1..100]

-- How many square roots of natural numbers does it take to sum up to 1000?

numRoots :: Int
-- Easier definition
-- numRoots = length (takeWhile (<1000) $ scanl1 (+) $ map sqrt [1..]) + 1
-- Readable definition
numRoots = 
    let sums = scanl1 (+) $ map sqrt [1..]
        listOfSums = takeWhile (<1000) sums
    in 1 + length listOfSums 
