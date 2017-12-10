module Main where

main = do
    input <- readFile "inputs/day09.txt"
    print $ parse input [] (0,0)

parse :: String -> [String] -> (Int, Int) -> (Int, Int)
parse [] _ score = score

-- Ignore any characters that are preceded by !
parse ('!':_:xs) zs score = parse xs zs score

-- Close garbage group
parse ('>':xs) (('<':ys):zs) score = parse xs zs score

-- Ignore any other characters within garbage group
parse (_:xs) all@(('<':ys):zs) (score, count) = parse xs all (score, count + 1)

-- Open garbage group
parse ('<':xs) ys score = parse xs ("<":ys) score

-- Open new group
parse ('{':xs) ys score = parse xs ("{":ys) score

-- Close group when current group starts with '{'
parse ('}':xs) (('{':ys):zs) (score, count) =
    parse xs zs ((score + 1 + (length zs)), count)

-- Ignore the rest
parse (_:xs) zs score = parse xs zs score
