module Main where

main :: IO ()
main = lines <$> readFile "input" >>= (putStrLn . show . totalFuel . fuelRequired . map read)
--                                                       part 2      part 1

formula :: Integer ->Integer
formula x = x `div` 3 - 2

fuelRequired :: [Integer] -> Integer
fuelRequired = sum . map formula

totalFuel :: Integer ->Integer
totalFuel initial
    | initial <= 0 = 0
    | otherwise    = initial + (totalFuel . formula) initial
