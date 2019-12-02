module Week1 where

-- parse string as integer
toInt :: String -> Int
toInt s = (read s :: Int)

-- the fuel requirement for a mass is floor(mass / 3) - 2
getFuelRequirement :: Int -> Int
getFuelRequirement mass = (mass `div` 3) - 2

main :: IO ()
main = do
    -- read input from stdin
    contents <- getContents
    -- parse lines as masses
    let masses = map toInt $ lines contents
    -- calculate fuel requirement per mass
    let fuels = map getFuelRequirement masses
    -- print sum of fuel requirements
    print $ sum fuels
