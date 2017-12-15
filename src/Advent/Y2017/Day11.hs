module Advent.Y2017.Day11 (day11a, day11b) where

import Data.List.Split (splitOn)

type Move   = String
type Vector = [Int]

origin :: Vector
origin = [0,0,0]

day11a, day11b :: String -> String
day11a input = show $ solve1 $ splitOn "," input
day11b input = show $ solve2 $ splitOn "," input

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
move _         _    = error "Unable to process move"