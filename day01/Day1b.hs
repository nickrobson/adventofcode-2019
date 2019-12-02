module Week1 where

-- parse string as integer
toInt :: String -> Int
toInt s = (read s :: Int)

-- the fuel requirement for a mass is floor(mass / 3) - 2
-- we need to also include the fuel requirement for this amount of fuel, though!
getFuelRequirement :: Int -> Int
getFuelRequirement mass =
    let fuel = (mass `div` 3) - 2 in
        if fuel <= 0 then 0
        else fuel + getFuelRequirement fuel

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
