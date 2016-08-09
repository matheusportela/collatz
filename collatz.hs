collatz :: Int -> [Int]
collatz x
    | x == 1 = [1]
    | otherwise = if (odd x)
                    then x : collatz (3*x + 1)
                    else x : collatz (x `div` 2)

largestCollatz :: [Int] -> (Int, Int)
largestCollatz xs = foldl
                        (\ acc x ->
                            let currentCollatzLength = length $ collatz x
                                previousCollatzLength = (snd acc)
                            in  if currentCollatzLength > previousCollatzLength
                                        then (x, currentCollatzLength)
                                        else acc)
                        (0, 0) xs
