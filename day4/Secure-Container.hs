module Main where

import Data.Maybe (isJust)
import Control.Monad ((>=>))

type Digits = [Int]

input :: String
input = "137683-596253"

parseInput :: String -> (Int, Int) 
parseInput s = (min, max)
    where min = read        $ takeWhile (/='-') s
          max = read . tail $ dropWhile (/='-') s

generatePasswords :: (Int, Int) -> [Digits]
generatePasswords (min, max) = map toDigits [min..max]
    where toDigits 0 = []
          toDigits n = toDigits (n `div` 10) ++ [n `mod` 10]

-- constraints --
hasSixDigits :: Digits -> Maybe Digits
hasSixDigits d = if (length . takeWhile (==0) $ d) == 0 then Just d else Nothing

nonDecreasingDigits :: Digits -> Maybe Digits
nonDecreasingDigits d = if snd . foldl (\(p,r) n -> (n, r && n>=p)) (0,True) $ d then Just d else Nothing

containsPairs :: Digits -> Digits -> Maybe Digits
containsPairs o []       = Nothing
containsPairs o d@(x:xs) = if (length . takeWhile (==x) $ xs) == 1 then Just o else containsPairs o xs

mustContainPairs :: Digits -> Digits -> Maybe Digits
mustContainPairs o []       = Nothing
mustContainPairs o d@(x:xs) = if (length . takeWhile (==x) $ xs) == 1 then Just o else mustContainPairs o (dropWhile (==x) xs)

noMoreThanPairs :: Digits -> Digits -> Maybe Digits
noMoreThanPairs o []        = Just o
noMoreThanPairs o d@(x:xs)  = if (length . takeWhile (==x) $ xs) > 1 then mustContainPairs o o else noMoreThanPairs o xs

splitA :: (a -> a -> b) -> a ->b
splitA f x = f x x

part1 :: [Digits] -> IO ()
part1 = print . length . filter isJust . map validate
    where validate = hasSixDigits >=> nonDecreasingDigits >=> splitA containsPairs

part2 :: [Digits] -> IO ()
part2 = print . length . filter isJust . map validate
    where validate = hasSixDigits >=> nonDecreasingDigits >=> splitA containsPairs >=> splitA noMoreThanPairs

main :: IO ()
main = let passwords = generatePasswords . parseInput $ input in
    part1 passwords >> part2 passwords

