module Day3 where

import Data.List
import Data.Maybe

type SpiralGrid = [Point]
data Point = Point { x :: Int, y :: Int } deriving (Show, Eq)
origin = Point 0 0

type SpiralGrid' = [Square]
data Square = Square { point :: Point, value :: Int } deriving (Show)
origin' = Square { point = origin, value = 1 }

data Move = Move { direction :: Direction , steps :: Int } deriving (Show)
data Direction = U | D | R | L deriving (Show, Eq)

main = do
    let input = 361527
    print $ part1 input
    print $ part2 input

part1 :: Int -> Int
part1 n = manhattanDistance origin (last (take n generateSpiralGrid))

manhattanDistance :: Point -> Point -> Int
manhattanDistance (Point x1 y1) (Point x2 y2) = (abs $ x2 - x1) + (abs $ y2 - y1)

generateSpiralGrid :: SpiralGrid
generateSpiralGrid = concat $ scanl applyMove [origin] generateMoves

applyMove :: [Point] -> Move -> [Point]
applyMove points move = walk move (last points)

walk :: Move -> Point -> [Point]
walk move point
    | d == U = map (\i -> Point { x = (x point)    , y = (y point) + i }) [1..n]
    | d == D = map (\i -> Point { x = (x point)    , y = (y point) - i }) [1..n]
    | d == R = map (\i -> Point { x = (x point) + i, y = y point       }) [1..n]
    | d == L = map (\i -> Point { x = (x point) - i, y = y point       }) [1..n]
    where d = direction move
          n = steps move

generateMoves :: [Move]
generateMoves = iterate getNextMove initialMove
initialMove = Move R 1

getNextMove :: Move -> Move
getNextMove previousMove
    | previousDirection == R = Move U previousSteps
    | previousDirection == U = Move L (previousSteps + 1)
    | previousDirection == L = Move D previousSteps
    | previousDirection == D = Move R (previousSteps + 1)
    where
          previousDirection = direction previousMove
          previousSteps     = steps previousMove

part2 :: Int -> Int
part2 n = value . fromJust
            $ find (\x -> value x > n)
            $ generateSpiralGrid' (drop 1 $ generateSpiralGrid) [origin']

generateSpiralGrid' :: [Point] -> [Square] -> [Square]
generateSpiralGrid' (x:xs) prevSquares = newPoint : generateSpiralGrid' xs (newPoint : prevSquares)
    where newPoint = (getNextSquare x prevSquares)

getNextSquare :: Point -> [Square] -> Square
getNextSquare p prev = Square { point = p
                              , value = sum . map value $ (filter (\q -> neighbours p (point q)) prev)
                              }

neighbours :: Point -> Point -> Bool
neighbours (Point x1 y1) (Point x2 y2) = (diffX == 0 || diffX == 1) && (diffY == 0 || diffY == 1)
    where diffX = abs $ x2 - x1
          diffY = abs $ y2 - y1
