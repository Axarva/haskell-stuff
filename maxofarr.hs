maxOfArr :: Ord a => [a] -> a
maxOfArr [x] = x
maxOfArr arr = if x > y
                 then x
               else maxOfArr $ tail $ quicksort arr
               where 
                x = head $ quicksort arr
                y = head $ tail $ quicksort arr

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
        let smallerSorted = quicksort [a | a <- xs, a <= x]  
            biggerSorted = quicksort [a | a <- xs, a > x]  
        in  smallerSorted ++ [x] ++ biggerSorted  