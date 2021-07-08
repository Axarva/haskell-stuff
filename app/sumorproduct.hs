doShit :: (Eq a, Num a) => a -> IO ()
doShit thing
    | thing == 1 = do
        putStrLn "Adding, enter number"
        x <- getLine
        let parsed = read x :: Int
        putStr "Your output is: "
        print (addIt parsed)
    | thing == 2 = do
        putStrLn "Multiplying, enter number"
        x <- getLine
        let parsed = read x :: Integer
        putStr "Your output is: "
        print (factorial parsed)
    | thing == 3 = putStrLn "Quitting."
    | otherwise = putStrLn "Dying."

addIt :: (Num a, Enum a) => a -> a
addIt n = sum [1..n]

factorial :: (Eq p, Num p) => p -> p
factorial 0 = 1
factorial n = n * factorial (n - 1)


main :: IO ()
main = do
    putStrLn "Hello! What would you like to do?"
    putStrLn "1) Get sum from 1 to n"
    putStrLn "2) Get factorial"
    putStrLn "3) Quit"
    num <- getLine
    doShit (read num :: Int)
