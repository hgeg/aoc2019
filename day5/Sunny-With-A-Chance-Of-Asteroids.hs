{-# LANGUAGE TypeApplications #-}
module Main where

import Data.Maybe (isNothing)
import Data.List (intercalate)
import Control.Monad ((<=<), (>=>))
import Control.Monad.Trans.Writer
import qualified Data.IntMap.Strict as M

type Mem = M.IntMap Int
data Program = Program Mem Int

mkProgram :: [String] -> Program
mkProgram source = Program (M.fromList . zip [0..] $ map read source) 0

split :: String -> [String]
split s = words $ [if c == ',' then  ' ' else c | c <- s]

getOp :: Int -> (Int, Int, Int, Int) 
getOp r = (op, m1, m2, m3)
    where op = r `mod` 100
          m3 = (r `div` 10000) `mod` 10
          m2 = (r `div`  1000) `mod` 10
          m1 = (r `div`   100) `mod` 10

write :: Int -> Int -> Mem -> Mem
write ptr val mem = M.insert (access 0 ptr mem) val mem

access :: Int -> Int -> Mem -> Int
access 0 ptr mem = mem M.! ptr
access 1 val _   = val

execute :: Program -> IO Program
execute (Program mem ptr) =
    case op of 
        -- part 1
        1  -> return $ Program (write (ptr+3) (param1 + param2) mem) (ptr+4)
        2  -> return $ Program (write (ptr+3) (param1 * param2) mem) (ptr+4)
        3  -> (readLn @Int) >>= (\val -> return $ Program (write (ptr+1) val mem) (ptr+2))
        4  -> print param1 >> return (Program mem (ptr+2))
        -- part 2
        5  -> return $ Program mem $ if param1 /= 0 then param2 else ptr+3
        6  -> return $ Program mem $ if param1 == 0 then param2 else ptr+3
        7  -> return $ Program (write (ptr+3) (if param1  < param2 then 1 else 0) mem) (ptr+4)
        8  -> return $ Program (write (ptr+3) (if param1 == param2 then 1 else 0) mem) (ptr+4)
        99 -> return $ Program mem (-1)
        _  -> error $ "invalid instruction: " ++ show (op, m1,m2,m3, ptr) ++ "\nmemory dump:\n" ++ dump
            
    where (op,m1,m2,m3) = getOp $ access 0 ptr mem
          param1        = access 0 (access m1 (ptr+1) mem) mem 
          param2        = access 0 (access m2 (ptr+2) mem) mem 
          dump          = intercalate "," . map (show . snd) . M.toList $ mem
    

run :: Program -> IO Program
run p@(Program mem ptr) 
    | isNothing $ M.lookup ptr mem = return p
    | otherwise                    = execute p >>= run

main :: IO ()
main = split <$> readFile "input" >>= (run . mkProgram) >> return ()
