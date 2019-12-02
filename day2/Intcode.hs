module Main where

import Data.List (intercalate)
import qualified Data.IntMap.Strict as M

data Source = Source Int (M.IntMap Int)

parse :: String -> Source
parse s =  Source (length split) $ M.fromList . zip [0..] . map read $ split
    where split = words [if c == ',' then  ' ' else c | c <- s]

output :: Source -> Int
output s = value 0 s 

write :: Int -> Int ->Source -> Source
write p v (Source l m) = (Source l $ M.insert p v m)

value :: Int -> Source -> Int
value p (Source l m) = case M.lookup p m of 
                Nothing -> 0
                Just v  -> v

execute :: Int -> Int -> Int ->Int
execute 1  x y = x + y
execute 2  x y = x * y
execute 99 _ _ = -1
execute _  _ _ = -1

eval :: Int -> Source -> Source
eval p s@(Source l m) 
    | p>=l            = s
    | value p s == 99 = s
    | otherwise       = eval (p+4) $ write z (execute op x y) s
        where (op, x, y, z) = (value p s, value (value (p+1) s) s, value (value (p+2) s) s, value (p+3) s)

try :: (Int, Int) -> Source -> Source
try (x , y) s = write 2 y $ write 1 x s 

fix :: Source -> Source
fix = try (12, 2)

part1 :: IO ()
part1 = do
    source <-fix <$> parse <$> readFile "input"
    let result = eval 0 source in 
        putStrLn . show . output $ result

part2 :: IO ()
part2 = do 
    source <-parse <$> readFile "input"
    let pairs = [(a, b) | a <- [0..99], b <- [0..99]]
    let trials = zip (repeat source) pairs
    let (noun, verb) = snd . head $ filter (\(s,p) ->(output . eval 0 $ try p s) == 19690720) trials
    putStrLn . show $ 100 * noun + verb
    

main = part1 >> part2
    
