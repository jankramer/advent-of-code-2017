module Day3 where

import Data.List
import Data.Maybe

data Move = Move { direction :: Direction
                 , steps :: Int
                 } deriving (Show, Eq)

data Direction = U | D | R | L deriving (Show, Eq)


data Point = Point { x :: Int
                   , y :: Int
                   } deriving (Show, Eq)
origin = Point { x = 0, y = 0 }

data ValuedPoint = ValuedPoint { point :: Point, value :: Int } deriving (Show)
origin' = ValuedPoint { point = origin, value = 1 }

main = do
    let input = 361527
    print $ part1 input
    print $ part2 input

part1 :: Int -> Int
part1 n = manhattanDistance origin (last (take n generateSpiral)) - 1

part2 :: Int -> Int
part2 n = value . fromJust $ find (\x -> value x > n) $ generateSequence generateSpiral [origin']

generateSequence :: [Point] -> [ValuedPoint] -> [ValuedPoint]
generateSequence (x:xs) valuedPoints = newPoint : generateSequence xs (newPoint : valuedPoints)
    where newPoint = (calculateValuedPoint x valuedPoints)


calculateValuedPoint :: Point -> [ValuedPoint] -> ValuedPoint
calculateValuedPoint p prev = ValuedPoint { point = p
                                          , value = sum . map value $ (filter (\q -> neighbours p (point q)) prev)
                                          }

neighbours :: Point -> Point -> Bool
neighbours (Point x1 y1) (Point x2 y2) = (diffX == 0 || diffX == 1) && (diffY == 0 || diffY == 1)
    where diffX = abs $ x2 - x1
          diffY = abs $ y2 - y1

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
