
--Ex 1
fact :: Int -> Int
fact 0 = 1
fact n 
    | n > 0 = n * (fact (n-1))
    |otherwise =  error "Negative input!"

--Ex 3
euclid :: Int -> Int -> Int
euclid x y | x == y = x
        | x < y = euclid x (y-x)
        |otherwise = euclid (x - y) y

--Ex 4
--a)
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs)  = 0 + x + sumList xs

--b)
takeElem :: [a] -> Int -> [a]
takeElem _ 0 = []
takeElem (x:xs) n = x : takeElem xs (n-1)

--c)
takeLast :: [a] -> a
takeLast [] = error "Cannot accept empty list"
takeLast [x] = x
takeLast (x:xs) = takeLast xs


--Ex 5
zipTogether :: [a] -> [b] -> [(a,b)]
zipTogether [] _ = []
zipTogether _ [] = []
zipTogether (x: xs) (y: ys) = (x,y) : zipTogether xs ys

--Ex 6
replicates :: Int -> a -> [a]
replicates 0 x = []
replicates n x = x : replicates (n-1) x

--Ex 7
elem' :: Ord a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs) | x == n = True
            |otherwise = elem' n xs

--Ex 8
selection :: [a] -> Int -> a
selection (x:_) 0 = x
selection (_:xs) n = selection xs (n-1)

--Ex 9
cantor :: Int -> [(Double, Double)] -> [(Double, Double)]
cantor 0 xs = xs
cantor n xs = cantor (n-1) (concatMap(\(x, y) -> [(x, x + ((y-x)/3)), (y - ((y-x)/3), y)]) xs)

--Ex 10
digitsOf :: Int -> [Int]
digitsOf x = reverse(digitsOf' x)

digitsOf' x | x< 10 = [x]
        |otherwise =  rem x 10 : digitsOf' (div x 10)

--Ex 11
--a)
oddSquaresc :: [Int] -> [Int]
--oddSquaresc xs = map (^2) (filter odd xs)
oddSquaresc xs = [x*x | x <- xs, odd x]

--b)
oddSquaresr :: [Int] -> [Int]
oddSquaresr [] = []
oddSquaresr (x:xs) 
                |odd x = x*x : oddSquaresr xs
                |otherwise = oddSquaresr xs
        
--Ex 12
--a)
inRangec :: Int -> Int -> [Int] -> [Int]
inRangec a b xs = [x | x <- xs, x >= a, x <= b]

--b)
inRanger :: Int -> Int -> [Int] -> [Int]
inRanger _ _ [] = []
inRanger a b (x:xs) | x >= a, x <= b = [x] ++ inRanger a b xs
                    |otherwise = inRanger a b xs

--Ex 13
--a)
factors :: Int -> [Int]
factors x = [a | a <- [1..x], x `mod` a == 0]

--b)
isPrime :: Int -> Bool
isPrime x | length (factors x) == 2 = True
        |otherwise = False
    
--c)
primeFactors :: Int -> [Int]
primeFactors x = [a | a <- factors x, isPrime a]