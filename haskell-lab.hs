max3 :: Integer -> Integer -> Integer -> Integer
max3 a b c = max a (max b c)

min3 :: Integer -> Integer -> Integer -> Integer
min3 a b c = min a (min b c)

sort2 :: Integer -> Integer -> (Integer, Integer)
sort2 a b = (min a b, max a b)

bothTrue' :: Bool -> Bool -> Bool
bothTrue' a b = (a, b) == (True, True)

solve2 :: Double->Double->(Bool,Double)
solve2 a b | a == 0 = (False, 0)
           | otherwise = (True,-b / a)

isParallel :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
isParallel (x1, y1) (x2, y2) (x1', y1') (x2', y2') =
  let dx = fromIntegral(x2 - x1)
      dy = fromIntegral(y2 - y1)
      dx' = fromIntegral(x2' - x1')
      dy' = fromIntegral(y2' - y1')
   in 
  not (x2 - x1 == 0 || x2' - x1' == 0) && (dy / dx) == (dy' / dx')

data Circle = Circle {x :: Double, y :: Double, radius :: Double}

isIncluded :: Circle -> Circle -> Bool
isIncluded c1 c2 = 
  let distance = sqrt ((x c1 - x c2) ^ 2 + (y c1 - y c2) ^2)
  in distance + radius c2 <= radius c1

isRectangular :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
isRectangular (x1, y1) (x2, y2) (x3, y3) =
  let a = distance (x1, y1) (x2, y2)
      b = distance (x1, y1) (x3, y3)
      c = distance (x2, y2) (x3, y3)
  in a == b + c || b == a + c || c == a + b

distance :: (Double, Double) -> (Double, Double) -> Double
distance (x1, y1) (x2, y2) = (x2 - x1)^2 + (y2 - y1)^2

isTriangle :: Double -> Double -> Double -> Bool
isTriangle x y z = x + y > z || x + z > y || y + z > x

isSorted :: Int -> Int -> Int -> Bool
isSorted a b c 
  | a > b && b > c = True
  | a < b && b < c = True 
  | otherwise = False
main :: IO()
main = do
  let circle1 = Circle { x = 0, y = 0, radius = 5 }
      circle2 = Circle { x = 0, y = 0, radius = 100 }
      result = isIncluded circle1 circle2
  print(isSorted 1 2 3)