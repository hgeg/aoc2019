module Main where

import qualified Data.Set as S

data Position = Pos (Int, Int) Int deriving (Show)
instance Eq Position where 
    (==) (Pos p1 _) (Pos p2 _) = p1 == p2
instance Ord Position where
    (<=) (Pos p1 _) (Pos p2 _) = p1 <= p2

data Cable = Cable (S.Set Position) Position

split :: String -> [String]
split s = words [if c == ',' then  ' ' else c | c <- s]

mkCable :: Cable -> [String] -> Cable
mkCable c           [] = c
mkCable (Cable s p) (x:xs) = mkCable (Cable (lay s p x) (update p x)) xs

lay :: (S.Set Position) -> Position -> String ->(S.Set Position)
lay s (Pos (x,y) z) (d:a) = S.union s (S.fromList $ laid d)
    where laid 'R' = [Pos (x+offset, y) (z+offset) | offset <- rng]
          laid 'L' = [Pos (x-offset, y) (z+offset) | offset <- rng]
          laid 'U' = [Pos (x, y+offset) (z+offset) | offset <- rng] 
          laid 'D' = [Pos (x, y-offset) (z+offset) | offset <- rng] 
          rng      = [0..read a]

update :: Position -> String -> Position
update (Pos (x, y) z) ('R':a) = Pos (x+read a, y) (z+read a)
update (Pos (x, y) z) ('L':a) = Pos (x-read a, y) (z+read a)
update (Pos (x, y) z) ('U':a) = Pos (x, y+read a) (z+read a)
update (Pos (x, y) z) ('D':a) = Pos (x, y-read a) (z+read a)

findClosest :: [Cable] -> Position
findClosest = foldr (\p1@(Pos (x,y) _) p2@(Pos (a,b) _) -> if abs(x)+abs(y) < abs(a)+abs(b) then p1 else p2) (Pos (9999999,99999999) 0) . allCrossed 
    where allCrossed ((Cable s1 _):(Cable s2 _):_) = S.delete (Pos (0,0) 0) (S.intersection s1 s2)

findLeastDelayed :: [Cable] ->(Position, Position)
findLeastDelayed = foldr (\(p1@(Pos a1 d1), p2@(Pos a2 d2)) (p3@(Pos a3 d3), p4@(Pos a4 d4)) -> if (a1==a2 && d1+d2 < d3+d4) then (p1, p2) else (p3,p4)) ((Pos (0,0) 9999999999999), (Pos (0,0) 9999999999999)) . allCrossed 
    where allCrossed ((Cable s1 _):(Cable s2 _):_) = (S.cartesianProduct (S.intersection (rmCenter s1) (rmCenter s2)) (S.intersection (rmCenter s2) (rmCenter s1)))
          rmCenter s = S.delete (Pos (0,0) 0) s 

part1 :: String -> String
part1 input = let cables = map (mkCable (Cable S.empty (Pos (0,0) 0)) . split) . lines $ input in show . findClosest $ cables

part2 :: String -> String
part2 input = let cables = map (mkCable (Cable S.empty (Pos (0,0) 0)) . split) . lines $ input in show . findLeastDelayed $ cables

main :: IO ()
main = readFile "input" >>= (\input -> (putStrLn . part1) input >> (putStrLn . part2) input)
