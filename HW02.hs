{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches guess actual = length $ filter (uncurry (==)) $ zip guess actual

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map (\x -> length ( filter (x == ) code )) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches guess actual = sum $ map (uncurry min) counts
  where counts = zip (countColors guess) (countColors actual)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove actual guess = Move guess exact nonExact
  where exact = exactMatches actual guess
        nonExact = matches actual guess - exact
        

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess exact nonExact) code  = exact == codeExact && nonExact == codeNonExact
  where codeExact = exactMatches guess code
        codeNonExact = matches guess code - codeExact

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (isConsistent move)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes n = loop n
  where loop 1 = map (: []) colors
        loop i = concatMap (\x -> map (:x) colors) $ loop (i-1)


-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve code = solveN code (allCodes (length code))

solveN :: Code -> [Code] -> [Move]
solveN code (guess:remaining)
    | isWinner (length code) next = [next]
    | otherwise = next : solveN code (filterCodes next remaining)
    where next = getMove code guess
solveN _ _ = []


isWinner :: Int -> Move -> Bool
isWinner n (Move _ exact _) = n == exact

-- Bonus ----------------------------------------------
-- 

fiveGuess :: Code -> [Move]
fiveGuess = undefined
