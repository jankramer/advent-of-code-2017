module Day1 where

import Data.Char
import Data.List

main = do
    input <- readFile "inputs/day1.txt"
    print $ solve 1                      (map digitToInt input)
    print $ solve (length input `div` 2) (map digitToInt input)

solve :: Int -> [Int] -> Int
solve offset digits = sum . map fst . filter matches . generatePairs offset $ digits

generatePairs :: Int -> [a] -> [(a, a)]
generatePairs offset input  = zip (map getFirstDigit range) (map getSecondDigit range)
    where range             = [0..length input-1]
          getFirstDigit i   = input !! i
          getSecondDigit i  = cycle input !! (i + offset)

matches :: Eq a => (a, a) -> Bool
matches (x, y) = x == y
