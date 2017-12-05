module Main where

import Data.Sequence (Seq)
import qualified Data.Sequence as Sequence

main = do
    instructions <- fmap (Sequence.fromList . map read . lines) $ readFile "inputs/day5.txt"
    print $ countSteps (+1) 0 0 instructions
    print $ countSteps (\x -> if x >= 3 then x - 1 else x + 1) 0 0 instructions

countSteps :: (Int -> Int) -> Int -> Int -> Seq Int -> Int
countSteps newPositionFn counter position instructions
    | position < 0 || position >= Sequence.length instructions = counter
    | otherwise = countSteps
        newPositionFn
        (counter+1)
        (position + currentInstruction)
        (Sequence.update position (newPositionFn currentInstruction) instructions)
    where currentInstruction = instructions `Sequence.index` position
