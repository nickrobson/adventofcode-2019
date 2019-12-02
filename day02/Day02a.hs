{-# LANGUAGE OverloadedStrings #-}

module Day02a where

import qualified Data.Text as T

-- parse string as integer
toInt :: String -> Int
toInt s = (read s :: Int)

-- run program until it halts
run :: ([Int], Int) -> [Int]
run (currentState, idx)
    -- when the opcode is 1, it's addition
    | currentState !! idx == 1  =
        -- read the next three numbers into x, y, z
        let x         = currentState !! (idx + 1)
            y         = currentState !! (idx + 2)
            z         = currentState !! (idx + 3)
            -- read the values at x and y, and add them together
            nextValue = currentState !! x + currentState !! y
            -- overwrite the value at z with the result
            nextState = take z currentState ++ (nextValue : drop (z + 1) currentState)
        -- run the next instruction
        in run (nextState, idx + 4)
    -- when the opcode is 2, it's multiplication
    | currentState !! idx == 2  =
        -- read the next three numbers into x, y, z
        let x         = currentState !! (idx + 1)
            y         = currentState !! (idx + 2)
            z         = currentState !! (idx + 3)
            -- read the values at x and y, and multiply them
            nextValue = currentState !! x * currentState !! y
            -- overwrite the value at z with the result
            nextState = take z currentState ++ (nextValue : drop (z + 1) currentState)
        -- run the next instruction
        in run (nextState, idx + 4)
    -- when the opcode is 99, we halt
    | currentState !! idx == 99 = currentState

main = do
    -- read input from stdin
    contents <- getContents
    -- split input on comma and map to integers
    let numbers = map (toInt . T.unpack) $ T.splitOn "," $ T.pack contents
    -- before running it, we're meant to replace the second and third values as 1202
    let original = numbers !! 0 : 12 : 2 : drop 3 numbers
    -- print the solution
    print $ run (original, 0) !! 0
