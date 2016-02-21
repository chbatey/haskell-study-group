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
  | otherwise = lastDigit i : toRevDigits (dropLastDigit oi)
--   | lastDigit i : toRevDigits (dropLastDigit i)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = undef

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = undefined


-- Exercise 5 -----------------------------------------

 -- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = undefined

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined

