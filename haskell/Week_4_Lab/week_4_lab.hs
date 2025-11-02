--Ex 1)
gradeBands :: [(String, Int)] -> (Int, Int, Int, Int, Int, Int)
gradeBands = foldr updateCounts initialCounts
    where
        initialCounts = (0, 0, 0, 0, 0, 0)

        updateCounts :: (String, Int) -> (Int, Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int, Int)
        updateCounts (_, score) (a, b, c, d, e, f)
            |score < 30 = (a + 1, b, c, d, e, f)
            |score >= 30 && score < 40 = (a, b+1, c, d, e, f)
            |score >= 40 && score < 50 = (a, b, c+1, d, e, f)
            |score >= 50 && score < 60 = (a, b, c, d+1, e, f)
            |score >= 60 && score < 70 = (a, b, c, d, e+1, f)
            |otherwise = (a, b, c, d, e, f+1)

--Ex 2)
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:).f) []

--b)
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

--Ex 4)
maximum' :: [Int] -> Int
maximum' (x:xs) = foldl max x xs

--Ex 5)
--a)
add3 :: [Int] -> [Int]
add3 xs = [x + 3 | x <- xs]

--b)
lessThan7 :: [Int] -> [Int]
lessThan7 xs = [x | x <- xs, x < 7]

--c)
concat' :: [a] -> [a] -> [(a, a)]
concat' xs ys = [(x, y) | x <- xs, y <- ys]

--concat (map (\x -> map (\y -> (x,y)) ys) xs)

--d)
filter'' :: [(Int, Int)] -> [Int]
filter'' xys = [uncurry (+) a | a <- xys, uncurry (+) a >3]

--Ex 6) It does literally nothing, its stupid and shouldnt exist

--Ex 7)

