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

-- get the state with the noun and verb set
setNounAndVerb :: [Int] -> Int -> Int -> [Int]
setNounAndVerb state noun verb = state !! 0 : noun : verb : drop 3 state

-- finds the noun and verb values necessary to hit a target value
findNounAndVerb :: [Int] -> Int -> (Int, Int)
findNounAndVerb initialState target = step initialState 0 0
    where
        step :: [Int] -> Int -> Int -> (Int, Int)
        step initialState noun verb =
            -- run the program with the initial state but with the noun and the verb swapped out
            let (x:_) = run (setNounAndVerb initialState noun verb, 0) in
                -- if we've hit our target, stop recursing
                if x == target then (noun, verb)
                -- if we've hit the max verb value (99), reset it and increment the noun value
                else if verb == 99 then step initialState (noun + 1) 0
                -- if we haven't hit the max verb value, increment it
                else step initialState noun (verb + 1)
                

main = do
    -- read input from stdin
    contents <- getContents
    -- split input on comma and map to integers
    let numbers = map (toInt . T.unpack) $ T.splitOn "," $ T.pack contents
    -- find the noun and verb values needed to get 19690720 as the first value
    let (noun, verb) = findNounAndVerb numbers 19690720
    -- print the solution
    print $ noun * 100 + verb
