module Main where

import Data.List

main = do
    input <- readFile "inputs/day02.txt"
    let spreadsheet = map (reverse . sort . map read . words) . lines $ input
    print $ solve rowChecksum1 spreadsheet
    print $ solve rowChecksum2 spreadsheet

solve :: ([Int] -> Int) -> [[Int]] -> Int
solve checksumFn spreadsheet = sum . map checksumFn $ spreadsheet

rowChecksum1 :: [Int] -> Int
rowChecksum1 values = head values - last values

rowChecksum2 :: [Int] -> Int
rowChecksum2 (x:xs)
    | null divisibles = rowChecksum2 xs
    | otherwise       = x `div` head divisibles
    where divisibles  = filter (\y -> x `mod` y == 0) xs
