-- PA5.hs
-- Ash Schultz
-- 2024-03-24
--
-- For CS 331 Spring 2024
-- Solutions to Assignment 5 Exercise B

module PA5 where


-- =====================================================================


-- collatzCounts
-- This is a list of Integer values. Item k (counting from zero) of collatzCounts tell how many iterations of
-- the Collatz function are required to take the k+1 number to 1. 
collatzCounts :: [Integer]
-- Call collatzLength 
collatzCounts = [collatzLength n | n <- [1..]]
  where
    collatzLength :: Integer -> Integer
    collatzLength n = toInteger (length (takeWhile (/= 1) (iterate collatzStep n)))

-- Collatzstep
-- defines the step function for the sequence
collatzStep :: Integer -> Integer
collatzStep n
    | even n    = n `div` 2
    | otherwise = 3 * n + 1


-- =====================================================================


-- operator <#
(<#) :: Ord a => [a] -> [a] -> Int
[] <# _ = 0 --Second list is empty
_ <# [] = 0 --First list is empty
(x:xs) <# (y:ys)
    | x < y = 1 + xs <# ys -- first array element is less then second array element
    | otherwise = xs <# ys -- == or greater or neither


-- =====================================================================


-- filter2
filter2 :: (a -> Bool) -> [a] -> [b] -> [b]
filter2 _ _ bs = bs


-- =====================================================================


-- listSearch
listSearch :: Eq a => [a] -> [a] -> Maybe Int
listSearch _ _ = Just 42  -- DUMMY; REWRITE THIS!!!


-- =====================================================================


-- concatEvenOdd
concatEvenOdd :: [String] -> (String, String)
{-
  The assignment requires concatEvenOdd to be written as a fold.
  Like this:

    concatEvenOdd xs = fold* ... xs  where
        ...

  Above, "..." should be replaced by other code. "fold*" must be one of
  the following: foldl, foldr, foldl1, foldr1.
-}
concatEvenOdd _ = ("Yo", "Yoyo")  -- DUMMY; REWRITE THIS!!!

