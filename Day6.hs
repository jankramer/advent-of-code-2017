module Main where

import              Data.Foldable
import              Data.Maybe
import              Data.Ord
import              Data.List
import              Data.Sequence (Seq)
import qualified    Data.Sequence as Sequence

type ReallocationStack  = [BlockAllocation]
type BlockAllocation    = Seq MemoryBank
data MemoryBank         = MemoryBank Index NumBlocks deriving (Show, Eq)
type Index              = Int
type NumBlocks          = Int
instance Ord MemoryBank where
    (MemoryBank posA valA) `compare` (MemoryBank posB valB)
        | valA > valB || (valA == valB && posA < posB) = GT
        | otherwise                                    = LT

main = do
    initialAllocation <- readInitialAllocation
    let stack = head $ dropWhile (not . headReoccurs) (iterate pushNextReallocation [initialAllocation])
    print $ length stack
    print $ succ $ fromJust $ let (x:xs) = stack in x `elemIndex` xs

readInitialAllocation :: IO BlockAllocation
readInitialAllocation = do
    numBlocks <- fmap (map read . words) $ readFile "inputs/day6.txt" :: IO [Int]
    return $ Sequence.mapWithIndex (\i x -> MemoryBank i x) (Sequence.fromList numBlocks)

pushNextReallocation :: ReallocationStack -> ReallocationStack
pushNextReallocation all@(current:previousAllocations) = (reallocate current) : all

reallocate :: BlockAllocation -> BlockAllocation
reallocate alloc = redistBlocks initial maxBankValue ((maxBankIndex+1) `mod` (length alloc))
    where (MemoryBank maxBankIndex maxBankValue) = maximum alloc
          initial = Sequence.update maxBankIndex (MemoryBank maxBankIndex 0) alloc

redistBlocks :: BlockAllocation -> NumBlocks -> Int -> BlockAllocation
redistBlocks currentAlloc remainingBlocks position
    | remainingBlocks == 0 = currentAlloc
    | otherwise            = redistBlocks nextAlloc (remainingBlocks-1) ((position + 1) `mod` length currentAlloc)
    where nextValue = let (MemoryBank i x) = Sequence.index currentAlloc position in succ x
          nextAlloc = Sequence.update position (MemoryBank position nextValue) currentAlloc

headReoccurs :: Eq a => [a] -> Bool
headReoccurs [] = False
headReoccurs (x:xs) = x `elem` xs
