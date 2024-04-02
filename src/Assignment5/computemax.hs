module Main where

import System.IO  -- for hFlush, stdout
import Text.Read (readMaybe)  -- Import readMaybe function from Text.Read

-- Function to safely convert string to Int
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
            putStrLn $ "Maximum value: " ++ show (maximum list)

    computeMax

    let tryAgain = do
            putStrLn "Would you like to try again? (y/n)"
            hFlush stdout
            again <- getLine

            if again == "y"
                then computeMax
                else return ()

    tryAgain
