{-
1.  Определите функцию, принимающую на вход целое число n и возвращающую список, 
    содержащий n элементов, упорядоченных по возрастанию.
-}

---------------------------------------------------------------------------------------------

{-
    1) Список натуральных чисел
-}

lst :: Int -> [Int]
lst 0 = []
lst n = lst (n - 1) ++ [n]

---list comp
lstComp :: Int -> [Int]
lstComp n = [1..n]

--map
lstMap :: Int -> [Int]
lstMap n = map (\x -> x) [1..n]

--foldr
lstFoldr :: Int -> [Int]
lstFoldr n = foldr (\x s -> x : s) [] [1..n]



{-  examples:

    input : lst 5
    output : [1,2,3,4,5]

    input : lstComp 5
    output : [1,2,3,4,5]

    input : lstMap 5
    output : [1,2,3,4,5]

    input : lstFoldr 5
    output : [1,2,3,4,5]

-}

---------------------------------------------------------------------------------------------

{-
    2) Список нечетных натуральных чисел.
-}

--Sorted list for odd nums
oddLst :: Int -> [Int]
oddLst 0 = []
oddLst n
    | even n = oddLst (n - 1)
    | otherwise = oddLst (n - 1) ++ [n]

--or list comp
oddLstComp :: Int -> [Int]
oddLstComp n = [x | x <- [1..n], odd x]

--foldr
oddLstFoldr :: Int -> [Int]
oddLstFoldr n = foldr (\x s -> if odd x then x : s else s) [] [1..n]

--filter

oddLstFilter :: Int -> [Int]
oddLstFilter n = filter odd [1..n]

{-  
    examples: 
    
    oddLst 5
    Output : [1,3,5,7,9]

    oddLstComp 5
    Output : [1,3,5,7,9]

    oddLstFoldr 5
    Output : [1,3,5,7,9]

    oddLstFilter 5
    Output : [1,3,5,7,9]
-}

---------------------------------------------------------------------------------------------

{-
    3) Список четных натуральных чисел.
-}

--Sorted list for even nums
evens :: Int -> [Int]
evens 0 = []
evens n
    | odd n = evens (n - 1)
    | otherwise = evens (n - 1) ++ [n]

--list comp
evenLstComp :: Int -> [Int]
evenLstComp n = [x | x <- [1..n], even x]

--foldr 
evenFoldr :: Int -> [Int]
evenFoldr n = foldr (\x s -> if even x then x : s else s) [] [1..n]

--filter
evenFilter :: Int -> [Int]
evenFilter n = filter even [1..n]

{-
    examples:

    input: evens 10
    output: [2,4,6,8,10]

    input: evenLstComp 10
    output: [2,4,6,8,10]

    input: evenFoldr 10
    output: [2,4,6,8,10]

    input: evenFilter 10
    output: [2,4,6,8,10]
-}

---------------------------------------------------------------------------------------------

{-
    4) Список квадратов натуральных чисел.
-}

--Sorted list square for nums
squareNums :: Int -> [Int]
squareNums 0 = []
squareNums n = squareNums (n - 1) ++ [n^2]

--lst comp
squareNumsLstComp :: Int -> [Int]
squareNumsLstComp n = [x^2 | x <- [1..n]]

--foldr 
squareNumsFoldr :: Int -> [Int]
squareNumsFoldr n = foldr (\x s -> x^2 : s) [] [1..n]

--map
squareNumsMap :: Int -> [Int]
squareNumsMap n = map (^2) [1..n]

{-
    examples:
    
    input: squareNums 10
    output: [1,4,9,16,25,36,49,64,81,100]

    input: squareNumsLstComp 10
    output: [1,4,9,16,25,36,49,64,81,100]

    input: squareNumsFoldr 10
    output: [1,4,9,16,25,36,49,64,81,100]

    input: squareNumsMap 10
    output: [1,4,9,16,25,36,49,64,81,100]
-}

---------------------------------------------------------------------------------------------

{-
    5)  Список факториалов.
-}

--Sorted list for factorial num
factNums :: Int -> [Int]
factNums 0 = []
factNums n = factNums (n - 1) ++ [factorial n]
    where
        factorial 0 = 1
        factorial n = factorial (n - 1) * n

--list comp
factLstComp :: Int -> [Int]
factLstComp n = [factorial x | x <- [1..n]]
    where factorial n = foldr (*) 1 [1..n]

--map
factMap :: Int -> [Int]
factMap n = map factorial [1..n]
    where
        factorial x = product [1..n]

--foldr 
factFoldr :: Int -> [Int]
factFoldr n = foldr (\x s -> factorial x : s) [] [1..n]
    where
        factorial n = product [1..n]

{-
    examples:
-}

---------------------------------------------------------------------------------------------

{-
    6) Список степеней двойки.
-}

--Sorted list for power of two num
sortedPowOfTwoNums :: Int -> [Int]
sortedPowOfTwoNums 0 = []
sortedPowOfTwoNums n = sortedPowOfTwoNums (n - 1) ++ [2 ^ n]

{-
    7) Список треугольных чисел.
-}

--Sorted list for triangular nums
sortedTriangularNums :: Int -> [Int]
sortedTriangularNums 0 = []
sortedTriangularNums n = sortedTriangularNums (n - 1) ++ [triangular n]

triangular :: Int -> Int
triangular 0 = 0
triangular n = n + triangular (n - 1)

{-
    8) Список пирамидальных чисел.
-}

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
    print (lstFoldr 10)