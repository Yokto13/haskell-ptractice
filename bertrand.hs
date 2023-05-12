isPrime::Int -> Bool
isPrime n 
    | (n == 2) = True
    | (n `mod` 2 == 0) = False
    | otherwise = isPrime' n 3
    where
        isPrime' n i
            | (n `mod` i == 0) = False
            | (i*i > n) = True
            | otherwise = isPrime' n (i+2)

bertrandL::Int -> [Int]
bertrandL n = [x | x <- [n+1..2*n], isPrime x]

bertrand::Int -> Int
bertrand n = head [x | x <- [n+1..2*n], isPrime x]