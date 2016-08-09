collatz :: Int -> [Int]
collatz x
    | x == 1 = [1]
    | otherwise = if (odd x)
                    then x : collatz (3*x + 1)
                    else x : collatz (x `div` 2)
