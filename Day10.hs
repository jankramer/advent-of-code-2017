module Main where

import Data.Bits        (xor)
import Data.Char        (ord)
import Data.List.Split  (splitOn)
import Text.Printf      (printf)

main = do
    print $ product $ take 2 result1
    print $ concat $ map formatHex $ calcHash result2

input       = "18,1,0,161,255,137,254,252,14,95,165,33,181,168,2,188"
listSize    = 256
list        = [0 .. (listSize-1)]

lengths1    = map read $ splitOn "," input
result1     = solve list lengths1 0 0

lengths2    = concat . replicate 64 $ map ord input ++ [17,31,73,47,23]
result2     = solve list lengths2 0 0

solve :: [Int] -> [Int] -> Int -> Int -> [Int]
solve list []     _   _    = list
solve list (y:ys) pos skip = solve newList ys newPos (skip + 1)
  where newList = go list pos y
        newPos  = (pos + y + skip) `mod` listSize

go :: [Int] -> Int -> Int -> [Int]
go list pos len = begin ++ end
  where (end, begin)    = splitAt (listSize - pos) $ (reverse sublist) ++ rest
        (sublist, rest) = splitAt len $ take listSize $ drop pos $ cycle list

calcHash :: [Int] -> [Int]
calcHash x
  | length x > 16 = hash : calcHash (drop 16 x)
  | otherwise     = [hash]
  where hash = foldl1 xor $ take 16 x

formatHex :: Int -> String
formatHex x = printf "%02x" x
