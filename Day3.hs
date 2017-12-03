module Day3 where

import Data.List

data Move = Move { direction :: Direction
                 , steps :: Int
                 } deriving (Show, Eq)

data Direction = U | D | R | L deriving (Show, Eq)


data Point = Point { x :: Int
                   , y :: Int
                   } deriving (Show, Eq)


main = do
    let input = 361527
    print $ solve input

solve :: Int -> Int
solve n = manhattanDistance Point { x = 0, y = 0 } (last (take n generateSpiral)) - 1

generateSpiral :: [Point]
generateSpiral = concat points
    where (_, points) = mapAccumL applyMove' Point{ x = 0, y = 0 } generateMoves

applyMove' :: Point -> Move -> (Point, [Point])
applyMove' previousPoint move = (last points, points)
    where points = applyMove move previousPoint

applyMove :: Move -> Point -> [Point]
applyMove move point
    | d == U = map (\i -> Point { x = (x point)    , y = (y point) + i }) [1..n]
    | d == D = map (\i -> Point { x = (x point)    , y = (y point) - i }) [1..n]
    | d == R = map (\i -> Point { x = (x point) + i, y = y point       }) [1..n]
    | d == L = map (\i -> Point { x = (x point) - i, y = y point       }) [1..n]
    where d = direction move
          n = steps move

generateMoves :: [Move]
generateMoves = iterate generateMove Move { direction = R, steps = 1 }

generateMove :: Move -> Move
generateMove previousMove
    | previousDirection == R = Move { direction = U, steps = previousSteps     }
    | previousDirection == U = Move { direction = L, steps = previousSteps + 1 }
    | previousDirection == L = Move { direction = D, steps = previousSteps     }
    | previousDirection == D = Move { direction = R, steps = previousSteps + 1 }
    where
          previousDirection = direction previousMove
          previousSteps     = steps previousMove

manhattanDistance :: Point -> Point -> Int
manhattanDistance (Point x1 y1) (Point x2 y2) = (abs $ x2 - x1) + (abs $ y2 - y1)
