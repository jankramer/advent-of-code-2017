module Main where

import Data.List.Split (splitOn)

type Move   = String
type Vector = [Int]
origin      = [0,0,0]

main = do
    moves <- splitOn "," <$> readFile "inputs/day11.txt"
    print $ solve1 moves
    print $ solve2 moves

solve1 :: [Move] -> Int
solve1 moves = distance origin (foldl move origin moves)

solve2 :: [Move] -> Int
solve2 moves = maximum (map (distance origin) (scanl move origin moves))

distance :: Vector -> Vector -> Int
distance a b = maximum $ map abs $ delta a b

delta :: Vector -> Vector -> Vector
delta a b = zipWith (-) b a

move :: Vector -> Move -> Vector
move (x:y:z:_) "nw" = [x-1, y+1,  z ]
move (x:y:z:_) "n"  = [ x , y+1, z-1]
move (x:y:z:_) "ne" = [x+1,  y , z-1]
move (x:y:z:_) "sw" = [x-1,  y , z+1]
move (x:y:z:_) "s"  = [ x , y-1, z+1]
move (x:y:z:_) "se" = [x+1, y-1,  z ]
