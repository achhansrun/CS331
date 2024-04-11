-- computemax.hs
-- Ash Schultz
-- 2024-04-01
--
-- For CS 331 Spring 2024
-- Solution to Assignment 5 Exercise C
-- Requires input to be Int values.
module Main where

import System.IO  -- for hFlush, stdout
import Text.Read -- for coverting string input to int

-- Function to convert string to Int
readInt :: String -> Maybe Int
readInt s = readMaybe s

main :: IO ()
main = do
    putStrLn "Running compute max.."
    putStrLn ""

    -- compute max
    let computeMax = do
            putStrLn "Input integers one line at a time on each line, then"
            putStrLn "I will return the maximum of those numbers"
            putStrLn ""
            -- returns the values from user
            let getValues values = do
                    putStrLn "Enter an integer: (blank to return max)"
                    hFlush stdout
                    line <- getLine

                    if line == ""
                        then return values
                        else case readInt line of
                                Just x -> getValues (x : values)
                                Nothing -> do
                                    putStrLn "Invalid input. Please enter a valid integer."
                                    getValues values
            list <- getValues []
            -- print the max of the list.
            putStrLn $ "Maximum value: " ++ show (maximum list)
    -- call compute max here
    computeMax
    --tryagain prompt so there is the ability to loop.
    let tryAgain = do
            putStrLn "Would you like to try again? (y/n)"
            hFlush stdout
            again <- getLine
            --if y then compute the max again. Otherwise return out of the program
            if again == "y"
                then computeMax
                else return ()

    tryAgain
