import Data.Char

--Ex3
choose :: (Ord a, Num a) => [(Bool, a)] -> [(Bool, a)]
choose = filter(\ (b, n) -> b && n < 10) 

--Ex 6
pyths :: Int -> [(Int, Int, Int)]
pyths a = [(x, y, z)| x <- [1..a], y <- [1..a], z <- [1..a], x^2 + y^2 == z^2]

--Ex 7
indices :: Eq a => a -> [a] -> [Int]
indices x xs | elem x xs = [n | (e, n) <- zip xs [0..], e == x]
            | otherwise = [] 

--Ex 8
luhnDouble :: Int -> Int
luhnDouble x |x*2 >9 = (x*2)-9
            |otherwise = x*2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = ((d + luhnDouble c + b + luhnDouble a) `mod` 10) == 0

--Ex 9
consecutiveSum :: [Int] -> [ Int]
consecutiveSum xs = map (\ (x, y) -> x+y) pairs
        where
            pairs = zip xs (tail xs)

--Ex 10
dotProduct :: [Int] -> [Int] -> Int
dotProduct xs ys = sum (map (\ (x, y) -> x*y) pairs)
        where
            pairs = zip xs ys

--Ex 11
inCircle :: Int -> Int
inCircle n = length(pairs)
        where
            pairs = [(x, y) | x <- [0..n], y <- [0..n], x^2 + y^2 < n^2]

--Ex 12
capitalizeFirst :: String -> String
capitalizeFirst str = toUpper(head str) : tail(str)

capitalize :: String -> String
capitalize str = unwords(map capitalizeFirst (words(str)))

--Ex 13
persons :: [(String, String, Int, Int)]
persons = [("Cervantes", "Literature", 1547, 1616),
    ("Velazquez", "Painting", 1599, 1660),
    ("Picasso", "Painting", 1881, 1973),
    ("Beethoven", "Music", 1770, 1823),
    ("Poincare", "Science", 1854, 1912),
    ("Quevedo", "Literature", 1580, 1654),
    ("Goya", "Painting", 1746, 1828),
    ("Einstein", "Science", 1879, 1955),
    ("Mozart", "Music", 1756, 1791),
    ("Botticelli", "Painting", 1445, 1510),
    ("Borromini", "Architecture", 1599, 1667),
    ("Bach", "Music", 1685, 1750)]

names :: [(String, String, Int, Int)] -> [String]
names xs = [a | (a,_,_,_) <- xs]

musicians :: [(String, String, Int, Int)] -> [String]
musicians xs = [a | (a, b,_,_) <- xs, b=="Music"]

select :: [(String, String, Int, Int)] -> String -> [String]
select xs activity = [a | (a, b,_,_) <- xs, b==activity]

musicians' :: [(String, String, Int, Int)] -> [String]
musicians' xs = select xs "Music"

alive :: [(String, String, Int, Int)] -> Int -> [String]
alive db a = [x| (x, _, c, d) <- db, c <= a, d >= a]