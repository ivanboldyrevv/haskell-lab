{-

1. Приведите пример нетривиальных выражений, принадлежащих сле-
дующему типу:

-}
---------------------------------------------------------------------------------------------
{-

  1) ((Char,Integer), String, [Double])

  ghci> let x = ((('a', 42), "Hello"), [1.0, 2.0, 3.0])
  ghci> :t x
  x :: (Fractional a, Num b) => (((Char, b), String), [a])

  2) [(Double,Bool,(String,Integer))]

  ghci> let x = [(2.1, True, ("Hello", 10))]
  ghci> :t x
  x :: (Fractional a, Num b) => [(a, Bool, (String, b))]

  3) ([Integer],[Double],[(Bool,Char)])

  ghci> let x = ([1], [2.2], [(True, 'c')])
  ghci> :t x
  x :: (Fractional a1, Num a2) => ([a2], [a1], [(Bool, Char)])    

  4) [[[(Integer,Bool)]]]

  ghci> let x = [[[(1, False)]]]
  ghci> :t x
  x :: Num a => [[[(a, Bool)]]]

  5) (((Char,Char),Char),[String])

  ghci> let x = ((('a', 'b'), 'c'), ["abc"])
  ghci> :t x
  x :: (((Char, Char), Char), [String])

  6) (([Double],[Bool]),[Integer])

  ghci> let x = (([10.12], [True]), [52])
  ghci> :t x
  x :: (Fractional a1, Num a2) => (([a1], [Bool]), [a2])

  7) [Integer, (Integer,[Bool])]

  ghci> let x = [192, (255, [False])]
  ghci> :t x
  x :: (Num a, Num (a, [Bool])) => [(a, [Bool])]

  8) (Bool,([Bool],[Integer]))

  ghci> let x = (True, ([False], [99]))
  ghci> :t x
  x :: Num a => (Bool, ([Bool], [a]))

  9) [([Bool],[Double])]

  ghci> :t x
  x :: Fractional a => [([Bool], [a])]

  10) [([Integer],[Char])]

  ghci> let x = [([10], ['A'])]
  ghci> :t x
  x :: Num a => [([a], [Char])]

-}

---------------------------------------------------------------------------------------------

{-

  2. Определите следующие функции:

-}

---------------------------------------------------------------------------------------------

{-

  1) Функция max3, по трем целым возвращающая наибольшее из
  них.

-}

max3 :: Integer -> Integer -> Integer -> Integer
max3 a b c = max a (max b c)

max3' :: (Ord a) => a -> a -> a -> a
max3' a b c 
  | a > b && a > c = a
  | b > a && b > c = c
  |otherwise = c

{-

  examples : 

    input : max3 100 42 (-99)
    output : 100

    input : max3' 100 42 (-99)
    output : 100



-}

---------------------------------------------------------------------------------------------

{-

  2) Функция min3, по трем целым возвращающая наименьшее из
  них.

-}

min3 :: Integer -> Integer -> Integer -> Integer
min3 a b c = min a (min b c)

min3' :: (Ord a) => a -> a -> a -> a
min3' a b c
  | a < b && a < c = a
  | b < a && b < c = b
  | otherwise = c

{-

  examples :

    input : min3 (-123) 1 23
    output : -123

    input : min3' (-123) 1 23
    output : -123

  # Стоит заметить, что функцию min3' можно применять к разным типам, т.к она полиморфна, а min3 мономорфна

-}

---------------------------------------------------------------------------------------------

{-

  3) Функция sort2, по двум целым возвращающая пару, в которой
  наименьшее из них стоит на первом месте, а наибольшее — на
  втором.

-}

sort2 :: Integer -> Integer -> (Integer, Integer)
sort2 a b = (min a b, max a b)

sort2' :: (Ord a) => a -> a -> (a, a)
sort2' a b
  | a > b = (b, a)
  |otherwise = (a, b)

{-

  examples : 
  
    input : sort2 2 1
    output : (1, 2)

    input : sort2' (-2.9) 1.2
    output : (-1.9, 1.2)

  # Также как и функция sort2' является полиморфной

-}

---------------------------------------------------------------------------------------------

{-

  4) Функция bothTrue :: Bool -> Bool -> Bool, которая
  возвращает True тогда и только тогда, когда оба ее аргумента
  будут равны True. Не используйте при определении функции
  стандартные логический операции (&&, || и т.п.).
  
-}

bothTrue' :: Bool -> Bool -> Bool
bothTrue' a b = (a, b) == (True, True)

{-

    examples : 

    input : bothTrue' True True
    output : True

    input : bothTrue' False False
    output : True

-}

---------------------------------------------------------------------------------------------

{-

  5) Функция solve2::Double->Double->(Bool,Double),
  которая по двум числам, представляющим собой коэффициенты
  линейного уравнения ax+ b= 0, возвращает пару, первый
  элемент которой равен True, если решение существует и False
  в противном случае; при этом второй элемент равен либо
  значению корня, либо 0.0.

-}

solve2 :: Double -> Double -> (Bool, Double)
solve2 a b | a == 0 = (False, 0)
           | otherwise = (True,-b / a)


{- 

  examples : 

    input : solve2 0.0 3.3
    output : (False,0.0)

    input : solve2 4 1.4
    output : (True,-0.35)


-}

---------------------------------------------------------------------------------------------

{-

  6) Функция isParallel, возвращающая True, если два отрез-
  ка, концы которых задаются в аргументах функции, параллель-
  ны (или лежат на одной прямой). Например, значение выраже-
  ния isParallel (1,1) (2,2) (2,0) (4,2) должно быть
  равно True, поскольку отрезки (1,1) −(2,2) и (2,0) −(4,2) па-
  раллельны.

-}

data Point = Point { x :: Double, y :: Double }

isParallel :: Point -> Point -> Point -> Point -> Bool
isParallel p1 p2 p3 p4 =
  let dx = x p2 - x p1
      dy = y p2 - y p1
      dx' = x p4 - x p3
      dy' = y p4 - y p3
  in not (dx == 0 || dx' == 0) && (dy / dx) == (dy' / dx')

{- 

  examples :

    let
      point1 = Point { x = 1, y = 1 }
      point2 = Point { x = 2, y = 2 }
      point3 = Point { x = 2, y = 0 }
      point4 = Point { x = 4, y = 2 }

    input : isParallel point1 point2 point3 point4
    output : True

    let
      point1 = Point { x = 0, y = 1 }
      point2 = Point { x = 2, y = -3 }
      point3 = Point { x = 2, y = 0 }
      point4 = Point { x = 4, y = 2 }

    input : isParallel point1 point2 point3 point4
    output : False 

-}

---------------------------------------------------------------------------------------------

{-

  7) Функция isIncluded, аргументами которой служат параметры
  двух окружностей на плоскости (координаты центров и радиусы);
  функция возвращает True, если вторая окружность целиком со-
  держится внутри первой.

-}

data Circle = Circle {x1 :: Double, y1 :: Double, radius :: Double}

isIncluded :: Circle -> Circle -> Bool
isIncluded c1 c2 = 
  let distance = sqrt ((x1 c1 - x1 c2) ^ 2 + (y1 c1 - y1 c2) ^2)
  in distance + radius c2 <= radius c1

{-

  examples : 

    let 
      circle1 = Circle {x1 = 10, y1 = 20, radius = 10}
      circle2 = Circle {x1 = 10, y1 = 20, radius = 5}
  
    input : isIncluded circle1 circle2
    output : True

    let 
      circle1 = Circle {x1 = 10, y1 = 20, radius = 10}
      circle2 = Circle {x1 = 10, y1 = 20, radius = 15}
  
    input : isIncluded circle1 circle2
    output : False

-}  


---------------------------------------------------------------------------------------------

{-

  8) Функция isRectangular, принимающая в качестве парамет-
  ров координаты трех точек на плоскости, и возвращающая True,
  если образуемый ими треугольник — прямоугольный.

-}

data Side = Side {side_x :: Double, side_y :: Double}

isRectangular :: Side -> Side -> Side -> Bool
isRectangular s1 s2 s3 =
  let a = distance (side_x s1, side_y s1) (side_x s2, side_y s2)
      b = distance (side_x s1, side_x s1) (side_x s3, side_y s3)
      c = distance (side_x s2, side_y s2) (side_x s3, side_y s3)
  in a == b + c || b == a + c || c == a + b

distance :: (Double, Double) -> (Double, Double) -> Double
distance (x1, y1) (x2, y2) = (x2 - x1)^2 + (y2 - y1)^2

{-

  examples : 

    let 
      side1 = Side {side_x = 5, side_y = 5}
      side2 = Side {side_x = 0, side_y = 5}
      side3 = Side {side_x = 5, side_y = 0}

    input : isRectangular side1 side2 side3
    output : True

    let 
      side1 = Side {side_x = 0, side_y = 0}
      side2 = Side {side_x = 2, side_y = 2}
      side3 = Side {side_x = 0, side_y = 5}

    input : isRectangular side1 side2 side3
    output : False

-}

---------------------------------------------------------------------------------------------

{-

  9) Функция isTriangle, определяющая, можно ли их отрезков с
  заданными длинами x, y и z построить треугольник.

-}

isTriangle :: (Ord a, Num a) => a -> a -> a -> Bool
isTriangle x y z = (x + y > z) && (x + z > y) && (y + z > x) && (x > 0) && (y > 0) && (z > 0)

{-

  example : 

    input : isTriangle 3 3 3
    output : True

    input : isTriangle 3 -3 3
    output : False


-}

---------------------------------------------------------------------------------------------

{-

  10) Функция isSorted, принимающая на вход три числа и возвра-
  щающая True, если они упорядочены по возрастанию или по
  убыванию.

-}

isSorted :: (Ord a, Num a) => a -> a -> a -> Bool
isSorted a b c 
  | a > b && b > c = True
  | a < b && b < c = True 
  | otherwise = False

{-

  example:

    input : isSorted 3 2 1
    output : True

    input : isSorted 3.2 2.9 2.8
    output : True

    input : isSorted 2.8 2.9 3.2
    output : True

    input : isSorted 2.8 3.2 2.9
    output : false

-}

main :: IO()
main = do
  print $ isSorted 2.8 2.9 3.2