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

--Sorted list square for nums
sortedSquareNums :: Int -> [Int]
sortedSquareNums 0 = []
sortedSquareNums n = sortedSquareNums (n - 1) ++ [n^2]

--Sorted list for factorial num
sortedFactNums :: Int -> [Int]
sortedFactNums 0 = []
sortedFactNums n = sortedFactNums (n - 1) ++ [factorial n]
    where
        factorial 0 = 1
        factorial n = factorial (n - 1) * n

--Sorted list for power of two num
sortedPowOfTwoNums :: Int -> [Int]
sortedPowOfTwoNums 0 = []
sortedPowOfTwoNums n = sortedPowOfTwoNums (n - 1) ++ [2 ^ n]

--Sorted list for triangular nums
sortedTriangularNums :: Int -> [Int]
sortedTriangularNums 0 = []
sortedTriangularNums n = sortedTriangularNums (n - 1) ++ [triangular n]

triangular :: Int -> Int
triangular 0 = 0
triangular n = n + triangular (n - 1)

--Sorted list for pyramid nums
sortedPyramidNums :: Int -> [Int]
sortedPyramidNums 0 = []
sortedPyramidNums n = sortedPyramidNums (n - 1) ++ [pyramid n]

pyramid :: Int -> Int
pyramid 0 = 0
pyramid n = triangular n + pyramid (n - 1)

--Func for avg real nums
avg :: [Double] -> Double
avg = uncurry (/) . go
    where go [] = (0,0)
          go (x:xs) = let (s,c) = go xs
                      in (s+x,c+1)

--Func for nums on index
getn :: Int -> [a] -> a
getn 0 (x:_) = x
getn n (_:xs) = getn (n-1) xs

--Sum of two lists
sum2 :: Num a => [a] -> [a] -> [a]
sum2 [] ys = ys
sum2 xs [] = xs
sum2 (x:xs) (y:ys) = (x + y) : sum2 xs ys

--removeOdd
removeOdd :: Integral a => [a] -> [a]
removeOdd = filter even

--removeEmpty
removeEmpty :: [String] -> [String]
removeEmpty = filter (not . null)

--countTrue
countTrue :: [Bool] -> Int
countTrue = length . filter id

--makePositive
makePositive :: Num a => [a] -> [a]
makePositive = map (\x -> abs x)

main :: IO()
main = do
    print (makePositive  [-1, 0, 5, -10, -20])