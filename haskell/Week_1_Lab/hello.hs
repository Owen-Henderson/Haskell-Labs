import Data.Char
n ::  Int
n = a `div` length xs
    where
        a = 10
        xs = [1, 2, 3, 4, 5]

halve :: [a] -> ([a], [a])
halve xs = (take ((length xs) `div` 2) xs, drop ((length xs) `div` 2) xs)

--Ex 7
euclidDist :: (Int, Int) -> (Int, Int) -> Float
euclidDist (x1, y1) (x2, y2) = sqrt(fromIntegral(addedDiffs))
    where   
        xDiff = (x2 - x1)
        yDiff = (y2 - y1)
        addedDiffs = xDiff ^ 2 + yDiff ^ 2

--Ex 8
firstWord :: String -> String
firstWord str = takeWhile (/= ' ') (dropWhile (==' ') str) 

--Ex 9
--a)
safeTail :: [a] -> [a]
safeTail list = if null list then [] else tail list
--b)
safeTail2 :: [a] -> [a]
safeTail2 list | null list = []
               | otherwise = tail list


--Ex 10
--a)
stack :: [a] -> [a]
stack list = (tail list)++[head list]

--b)
range :: Int -> Bool
range num = num <= 10 && num >= 0 

--c)
addc :: Char -> String -> String
addc char str = char : str

--d)
halves :: Fractional a => [a] -> [a]
halves intList = map(/2) intList

--e)
capitalizeStart :: String -> String
capitalizeStart str = [toUpper(head str)] ++ tail str

--Ex 11
oddItems :: [Int] -> [Int]
oddItems list = filter odd list

--Ex 12
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree x y z = max (max x y) z

--Ex 13
rotate :: Int -> [Int] -> [Int]
rotate n xs = snd splitList ++ fst splitList
    where
        splitList = splitAt n xs

--Ex 14
--a)
threeEqual :: Int -> Int -> Int -> Bool
threeEqual x y z = x == y && y == z
--b)
fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual w x y z = threeEqual w x y && y == z

--Ex 15
quadrant ::  (Num a, Ord a) => (a, a) -> Int
quadrant coord
             |fst coord > 0 && snd coord > 0 = 1
             |fst coord < 0 && snd coord > 0 = 2
             |fst coord < 0 && snd coord < 0 = 3
             |fst coord > 0 && snd coord < 0 = 4

--Ex 16
solutions :: (Ord a, Floating a) => a -> a -> a -> (a, a)
solutions a b c = ((-b + sqrt(discriminant))/2*(a), (-b - sqrt(discriminant))/2*(a))
    where
        discriminant = b^2 - 4*(a)*(c)
            
