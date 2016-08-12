collatz :: Int -> [Int]
collatz 1 = [1]
collatz x = if (odd x)
                then x : collatz (3*x + 1)
                else x : collatz (x `div` 2)

toOne :: Int -> [Int]
toOne 1 = [1]
toOne x = x : toOne (x-1)

seqToCollatzLength :: [Int] -> (Int, Int)
seqToCollatzLength xs = foldl
                        (\ acc x ->
                            let currentLength = length $ collatz x
                                previousLength = (snd acc)
                            in  if currentLength > previousLength
                                        then (x, currentLength)
                                        else acc)
                        (0, 0) xs

largestCollatzNumAndLength :: Int -> (Int, Int)
largestCollatzNumAndLength = seqToCollatzLength . toOne

largestCollatzLength :: Int -> Int
largestCollatzLength = snd . largestCollatzNumAndLength

largestCollatzNum :: Int -> Int
largestCollatzNum = fst . largestCollatzNumAndLength

largestCollatz :: Int -> [Int]
largestCollatz = collatz . largestCollatzNum
