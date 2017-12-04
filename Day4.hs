module Day4 where

import Data.List

main = do
    input <- readFile "inputs/day4.txt"
    print $ length $ filter (not . containsDuplicates) . map words $ lines input
    print $ length $ filter (not . containsAnagrams) . map words $ lines input

containsDuplicates :: [String] -> Bool
containsDuplicates passwords = (length $ nub passwords) < length passwords

containsAnagrams :: [String] -> Bool
containsAnagrams [] = False
containsAnagrams (x:xs) = length (filter (isAnagram x) xs) > 0 || containsAnagrams xs

isAnagram :: [Char] -> [Char] -> Bool
isAnagram x y = (sort x) == (sort y)
