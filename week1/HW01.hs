{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Digit length
digitLength :: Integer -> Integer
digitLength n = toInteger $ length (show n)

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit i = i `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit i = i `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits i
  | i <= 0 = []
  | otherwise = lastDigit i : toRevDigits (dropLastDigit i)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = [x, 2 * y] ++ doubleEveryOther xs 

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | digitLength x == 1 = x + sumDigits xs
  | otherwise = (lastDigit x) + (dropLastDigit x) + sumDigits xs


-- Exercise 5 -----------------------------------------

 -- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = (== 0) . (`mod` 10) . sumDigits  .  doubleEveryOther . toRevDigits 

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n start end temp = hanoi (n-1) start temp end ++ [(start,end)] ++ hanoi (n-1) temp end start

    

