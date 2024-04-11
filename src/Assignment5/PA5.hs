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
filter2 :: (t -> Bool) -> [t] -> [a] -> [a]
filter2 _ [] _ = []  -- If the first list is empty, return an empty list
filter2 _ _ [] = []  -- If the second list is empty, return an empty list
filter2 pred (x:xs) (y:ys) =
    if pred x --apply predicate to head of list
    then y: filter2 pred xs ys -- if true then append the current index y value to return list
    else filter2 pred xs ys --else move through to the next value in both.


-- =====================================================================
--listsearch
listSearch :: Eq a => [a] -> [a] -> Maybe Int
listSearch xs ys = searchformatch xs ys 0

searchformatch :: Eq a => [a] -> [a] -> Int -> Maybe Int
searchformatch [] _ index = Just index --return the index once done
searchformatch _ [] _ = Nothing --Return nothing if not found or null lists
searchformatch (x:xs) (y:ys) index 
    | match (x:xs) (y:ys) = Just index 
    | otherwise = searchformatch (x:xs) ys (index + 1)

match :: Eq a => [a] -> [a] -> Bool
match [] _ = True
match _ [] = False
match (x:xs) (y:ys)
    | x == y = match xs ys
    | otherwise = False
-- =====================================================================


-- concatEvenOdd
{-
  The assignment requires concatEvenOdd to be written as a fold.
  Like this:

    concatEvenOdd xs = foldr xs  where
        ...

  Above, "..." should be replaced by other code. "fold*" must be one of
  the following: foldl, foldr, foldl1, foldr1.
-}
concatEvenOdd :: [String] -> (String, String)
concatEvenOdd xs = foldr (\(i, x) (evenAcc, oddAcc) ->
                            if even i
                            then (x ++ evenAcc, oddAcc)
                            else (evenAcc, x ++ oddAcc))
                         ("", "")
                         (zip [0..] xs)

