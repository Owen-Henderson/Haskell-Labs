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
map :: (a -> a) -> [a] -> [a]
map = 
