module Day2 where

main = do
    input <- readFile "inputs/day2.txt"
    let spreadsheet = map (map read . words) . lines $ input
    print $ solve rowChecksum1 spreadsheet
    print $ solve rowChecksum2 spreadsheet

solve :: ([Int] -> Int) -> [[Int]] -> Int
solve checksumFn spreadsheet = sum . map checksumFn $ spreadsheet

rowChecksum1 :: [Int] -> Int
rowChecksum1 values = (maximum values) - (minimum values)

rowChecksum2 :: [Int] -> Int
rowChecksum2 (x:xs)
    | length divisibles == 0    = rowChecksum2 xs
    | otherwise                 = (max x y) `div` (min x y)
    where divisibles = filter (isDivisiblePair x) xs
          y          = head divisibles

isDivisiblePair :: Int -> Int -> Bool
isDivisiblePair a b = (max a b) `mod` (min a b) == 0
