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


lstComp :: Int -> [Int]
lstComp n = [1..n]


lstMap :: Int -> [Int]
lstMap n = map (\x -> x) [1..n]


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

oddLst :: Int -> [Int]
oddLst 0 = []
oddLst n
    | even n = oddLst (n - 1)
    | otherwise = oddLst (n - 1) ++ [n]


oddLstComp :: Int -> [Int]
oddLstComp n = [x | x <- [1..n], odd x]


oddLstFoldr :: Int -> [Int]
oddLstFoldr n = foldr (\x s -> if odd x then x : s else s) [] [1..n]


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

evens :: Int -> [Int]
evens 0 = []
evens n
    | odd n = evens (n - 1)
    | otherwise = evens (n - 1) ++ [n]


evenLstComp :: Int -> [Int]
evenLstComp n = [x | x <- [1..n], even x]


evenFoldr :: Int -> [Int]
evenFoldr n = foldr (\x s -> if even x then x : s else s) [] [1..n]


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


squareNums :: Int -> [Int]
squareNums 0 = []
squareNums n = squareNums (n - 1) ++ [n^2]


squareNumsLstComp :: Int -> [Int]
squareNumsLstComp n = [x^2 | x <- [1..n]]


squareNumsFoldr :: Int -> [Int]
squareNumsFoldr n = foldr (\x s -> x^2 : s) [] [1..n]


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


factNums :: Int -> [Int]
factNums 0 = []
factNums n = factNums (n - 1) ++ [factorial n]
    where
        factorial 0 = 1
        factorial n = factorial (n - 1) * n


factLstComp :: Int -> [Int]
factLstComp n = [factorial x | x <- [1..n]]
    where factorial n = foldr (*) 1 [1..n]


factMap :: Int -> [Int]
factMap n = map factorial [1..n]
    where
        factorial x = product [1..n]


factFoldr :: Int -> [Int]
factFoldr n = foldr (\x s -> factorial x : s) [] [1..n]
    where
        factorial n = product [1..n]

{-
    examples:

        input : factNums 10
        output : [1,2,6,24,120,720,5040]

        input : factLstComp 10
        output : [1,2,6,24,120,720,5040]

        input : factMap 10
        output : [1,2,6,24,120,720,5040]

        input : factFoldr 10
        output : [1,2,6,24,120,720,5040]

-}

---------------------------------------------------------------------------------------------

{-
    6) Список степеней двойки.
-}


powOfTwoNums :: Int -> [Int]
powOfTwoNums 0 = []
powOfTwoNums n = powOfTwoNums (n - 1) ++ [2 ^ n]


powLstComp :: Int -> [Int]
powLstComp n = [2 ^ x | x <- [1..n]]



powMap :: Int -> [Int]
powMap n = map (\x -> 2 ^ x) [1..n]


powFoldr :: Int -> [Int]
powFoldr n = foldr (\x s -> 2 ^ x : s) [] [1..n]

{-

    examples:

        input : powOfTwoNums 10
        output : [2,4,8,16,32,64,128,256,512,1024]

        input : powLstComp 10
        output : [2,4,8,16,32,64,128,256,512,1024]

        input : powMap 10
        output : [2,4,8,16,32,64,128,256,512,1024]

        input : powFoldr 10
        output : [2,4,8,16,32,64,128,256,512,1024]

-}

---------------------------------------------------------------------------------------------

{-
    7) Список треугольных чисел.
-}

--triangular func fon num_n in triangular numbers
triangular :: Int -> Int
triangular n = sum [1..n]


triangularNums :: Int -> [Int]
triangularNums 0 = []
triangularNums n = triangularNums (n - 1) ++ [triangular n]


triangularLstComp :: Int -> [Int]
triangularLstComp n = [triangular x | x <- [1..n]]


triangularMap :: Int -> [Int]
triangularMap n = map triangular [1..n]


triangularFoldr :: Int -> [Int]
triangularFoldr n = foldr (\x s -> triangular x : s) [] [1..n]

{-

    examples :

        input : triangularNums 4
        output : [1,3,6,10]

        input : triangularLstComp 4
        output : [1,3,6,10]

        input : triangularMap 4
        output : [1,3,6,10]

        input : triangularFoldr 4
        output : [1,3,6,10]


-}

---------------------------------------------------------------------------------------------

{-
    8) Список пирамидальных чисел.
-}


sortedPyramidNums :: Int -> [Int]
sortedPyramidNums 0 = []
sortedPyramidNums n = sortedPyramidNums (n - 1) ++ [pyramid n]

pyramid :: Int -> Int
pyramid 0 = 0
pyramid n = triangular n + pyramid (n - 1)

---------------------------------------------------------------------------------------------

{-

    2. Определите следующие функции:

-}

---------------------------------------------------------------------------------------------

{-

    1) Функция, принимающая на входе список вещественных чисел и
    вычисляющую их арифметическое среднее. Постарайтесь, чтобы
    функция осуществляла только один проход по списку.

-}

average :: [Double] -> Double
average xs = total / fromIntegral n
    where
        (total, n) = foldr (\x (sumAcc, count) -> (sumAcc + x, count + 1)) (0, 0) xs

{-

    examples :

        input : average [1, 2, 3, 5.5, 9.2]
        output : 4.14

-}

---------------------------------------------------------------------------------------------
{-

    2) Функция вычленения n-го элемента из заданного списка.

-}

getOnIndex :: Int -> [a] -> a
getOnIndex 0 (x:_) = x
getOnIndex n (_:xs) = getOnIndex (n-1) xs

{-

    examles : 

        input : getOnIndex 3 [11, 23, 54, 98, 19]
        output : 98

-}

---------------------------------------------------------------------------------------------
{-

    3) Функция сложения элементов двух списков. Возвращает список,
    составленный из сумм элементов списков-параметров. Учесть,
    что переданные списки могут быть разной длины.

-}

sum3 :: Num a => [a] -> [a] -> [a]
sum3 = zipWith (+)

{-

    examples :

        input : sum3 [1, 0] [3, 4, 5]
        output : [4,4]

-}

---------------------------------------------------------------------------------------------

{-

    4) Функция перестановки местами соседних четных и нечетных
    элементов в заданном списке

-}

swap :: [Int] -> [Int]
swap [] = []
swap (x:y:xs)
    | even x && odd y = y : x : swap xs
    | odd x && even y = y : x : swap xs
    | otherwise = x : swap (y:xs)

{-

    examples: 

        input : swap [1..10]
        output : [2,1,4,3,6,5,8,7,10,9]

        input : swap [1, 2, 3, 2]
        output : [2,1,2,3]


-}
---------------------------------------------------------------------------------------------

{-

    5) Функция twopow n, которая вычисляет 2n, исходя из следую-
    щих соображений. Пусть необходимо возвести 2 в степень n.
    Если nчетно, т.е. n= 2k, то 2n= 22k= (2k)2. Если nнечетно,
    т.е. n= 2k+ 1, то 2n= 22k+1 = 2 ·(2k)2. Функция twopow не
    должна использовать оператор ^ или любую функцию возведения
    в степень из стандартной библиотеки. Количество рекурсивных
    вызовов функции должно быть пропорционально log n.

-}

twopowR :: [Int] -> [Int]
twopowR [] = []
twopowR (x:xs) = pow2 x : twopowR xs

twopow :: [Int] -> [Int]
twopow = map (\ x -> pow2 x)

pow2 :: Int -> Int
pow2 n
    | n == 1 = 2
    | even n = w * w
    | otherwise = (w * w) + (w * w)
        where
            w = pow2 $ n `div` 2

{-

    examples :

        input : twopow [1..10]
        output : [2,4,8,16,32,64,128,256,512,1024]

        input : twopowR [1..10]
        output : [2,4,8,16,32,64,128,256,512,1024]

-}
---------------------------------------------------------------------------------------------

{-

    6) Функция removeOdd, которая удаляет из заданного
    списка целых чисел все нечетные числа. Например:
    removeOdd [1,4,5,6,10] должен возвращать [4,6,10].

-}

removeOdd :: Integral a => [a] -> [a]
removeOdd = filter even

removeOddR :: [Int] -> [Int]
removeOddR [] = []
removeOddR (x:xs)
    | odd x = removeOddR xs
    | otherwise = x : removeOddR xs

removeOddFoldr :: [Integer] -> [Integer]
removeOddFoldr = foldr (\x s -> if even x then x : s else s) []

{-

    examples : 
    
        input : removeOdd [1,4,5,6,10]
        output : [4,6,10]

        input : removeOddR [1,4,5,6,10]
        output : [4,6,10]

        input : removeOddFoldr [1,4,5,6,10]
        output : [4,6,10]

-}

---------------------------------------------------------------------------------------------
{-

    7) Функция removeEmpty, которая удаляет пустые строки из за-
    данного списка строк. Например:
    removeEmpty ["", "Hello", "", "", "World!"]
    возвращает ["Hello","World!"].

-}

removeEmpty :: [String] -> [String]
removeEmpty = filter (not . null)

removeEmptyFoldr :: [String] -> [String]
removeEmptyFoldr = foldr (\x s -> if x == "" then s else x : s) []

removeEmptyR :: [String] -> [String]
removeEmptyR [] = []
removeEmptyR (x:xs)
    | x == "" = removeEmptyR xs
    | otherwise = x : removeEmptyR xs

{-

    examples : 

        input : removeEmpty ["", "Hello", "", "", "World!"]
        output : ["Hello","World!"]

        input : removeEmptyFoldr ["", "Hello", "", "", "World!"]
        output : ["Hello","World!"]

        input : removeEmptyR ["", "Hello", "", "", "World!"]
        output : ["Hello","World!"]

-}

---------------------------------------------------------------------------------------------

{-

    8) Функция countTrue :: [Bool] -> Integer, возвращаю-
    щая количество элементов списка, равных True.

-}

countTrue :: [Bool] -> Int
countTrue = length . filter id

countTrueR :: [Bool] -> Int
countTrueR [] = 0
countTrueR (x:xs) = count + countTrueR xs
    where
        count = if x then 1 else 0

countTrueFoldr :: [Bool] -> Int
countTrueFoldr = foldr (\x s -> if x then s + 1 else s) 0

{-

    examples : 

        let xs = [True, False]
        let trueList = concat $ replicate 5 xs

        input : countTrue trueList
        output : 5

        input : countTrueR trueList
        output : 5

        input : countTrueFoldr trueList
        output : 5

-}
---------------------------------------------------------------------------------------------

{-

    9) Функция makePositive, которая меняет знак
    всех отрицательных элементов списка чисел, напри-
    мер: makePositive [-1, 0, 5, -10, -20] дает
    [1,0,5,10,20]

-}

makePositive :: Num a => [a] -> [a]
makePositive = map abs

makePositiveR :: (Num a, Ord a) => [a] -> [a]
makePositiveR [] = []
makePositiveR (x:xs)
    | 0 > x = -x : makePositiveR xs
    | otherwise = x : makePositiveR xs

makePositiveFoldr :: (Num a, Ord a) => [a] -> [a]
makePositiveFoldr = foldr (\x s -> if 0 > x then -x : s else x : s) []

{-

    examples :

        input : makePositive [-1, -9, -10, 5, 10]
        output : [1,9,10,5,10]

        input : makePositiveR [-1, -9, -10, 5, 10]
        output : [1,9,10,5,10]

        input : makePositiveFoldr [-1, -9, -10, 5, 10]
        output : [1,9,10,5,10]

-}

---------------------------------------------------------------------------------------------

{-

    10) Функция delete :: Char -> String -> String, кото-
    рая принимает на вход строку и символ и возвращает
    строку, в которой удалены все вхождения символа. При-
    мер: delete ’l’ "Hello world!" должно возвращать "Heo
    word!".

-}

delete :: Char -> String -> String
delete _ [] = []
delete n (x:xs)
    | n == x = delete n xs
    | otherwise = x : delete n xs

deleteFilter :: Char -> String -> String
deleteFilter n = filter (/= n)

deleteFoldr :: Char -> String -> String
deleteFoldr n = foldr (\x s -> if x == n then s else x : s) []

{-

    examples : 

        input : delete 'l' "Hello, World!"
        output : "Heo, Word!"

        input : deleteFilter 'l' "Hello, World!"
        output : "Heo, Word!"

        input : deleteFoldr 'l' "Hello, World!"
        output : "Heo, Word!"

-}

---------------------------------------------------------------------------------------------

{-

    11) Функция substitute :: Char -> Char -> String -> String,
    которая заменяет в строке указанный символ на заданный. При-
    мер: substitute ’e’ ’i’ "eigenvalue" возвращает
    "iiginvalui"

-}

substitute :: Char -> Char -> String -> String
substitute _ _ [] = []
substitute f s (x:xs)
    | f == x = s : substitute f s xs
    | otherwise = x : substitute f s xs

substituteFoldr :: Char -> Char -> String -> String
substituteFoldr f q = foldr (\x s -> if x == f then q : s else x : s) []

substituteMap :: Char -> Char -> String -> String
substituteMap f s = map (\x -> if x == f then s else x)

{-

    examples :

        input : substitute 'e' 'i' "eigenvalue"
        output : "iiginvalui"

        input : substituteFoldr 'e' 'i' "eigenvalue"
        output : "iiginvalui"

        input : substituteMap 'e' 'i' "eigenvalue"
        output : "iiginvalui"


-}

---------------------------------------------------------------------------------------------


main :: IO()
main = do
    print (swap [1, 2, 3, 2])