-- This is a pure function that takes two integers (a and b)
-- and returns an integer which is the sum of their squares.
-- The type signature 'Int -> Int -> Int' means:
-- it takes an Int, then another Int, and returns an Int.
sumOfSquares :: Int -> Int -> Int
sumOfSquares a b = (a * a) + (b * b)

-- The main function now uses 'do' notation to sequence multiple IO actions.
main :: IO ()
main = do
  -- Use 'let' to define local, immutable variables.
  let x = 5
      y = 12
      result = sumOfSquares x y
  
  -- putStrLn prints a line of text.
  -- '$' is a function application operator (for cleaner syntax).
  -- 'show' converts the number (Int) to a string so it can be concatenated (++)
  putStrLn $ "Calculating the sum of squares for " ++ show x ++ " and " ++ show y ++ "..."
  putStrLn $ "The hypotenuse of a right triangle with those sides is: " ++ show result
  putStrLn "Wait, I forgot to take the square root!"