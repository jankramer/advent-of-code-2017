module Main where

import Data.Graph       (components, graphFromEdges, reachable)
import Data.List.Split  (splitOn)

main = do
    (g, _, _) <- graphFromEdges <$> map parse <$> lines <$> readFile "inputs/day12.txt"
    print $ length $ reachable g 0
    print $ length $ components g

parse :: String -> (Int, Int, [Int])
parse line = (id, id, connections)
    where (x:xs:_)    = splitOn " <-> " line
          connections = map read $ splitOn "," xs
          id          = read x
