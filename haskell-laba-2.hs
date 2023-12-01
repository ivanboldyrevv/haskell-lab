sortedLst :: Int -> [Int]
sortedLst 0 = []
sortedLst n = sortedLst (n - 1) ++ [n]
---or simple method
sortedLst' :: Int -> [Int]
sortedLst' n = [1..n]

--Sorted list for odd nums
sortedOddLst :: Int -> [Int]
sortedOddLst 0 = []
sortedOddLst n
    | even n = sortedOddLst (n - 1)
    | otherwise = sortedOddLst (n - 1) ++ [n]

--Sorted list for even nums
sortedEvenLst :: Int -> [Int]
sortedEvenLst 0 = []
sortedEvenLst n
    | odd n = sortedEvenLst (n - 1)
    | otherwise = sortedEvenLst (n - 1) ++ [n]


main :: IO()
main = do
    print (sortedEvenLst 10)