module Main where

type Firewall = [Layer]
type Layer    = (Int, Int)

main = do
    firewall <- map parse <$> lines <$> readFile "inputs/day13.txt"
    print $ day13a firewall
    print $ day13b firewall

day13a :: Firewall -> Int
day13a fw = sum [d * r | (d, r) <- fw, scannerAtTop d r]

day13b :: Firewall -> Int
day13b fw = head [delay | delay <- [0..], undetected fw delay]

scannerAtTop :: Int -> Int -> Bool
scannerAtTop _ 1 = True
scannerAtTop time range = time `mod` (2 * (range - 1)) == 0

undetected :: Firewall -> Int -> Bool
undetected fw delay = and [not $ scannerAtTop (delay + d) r | (d, r) <- fw]

parse :: String -> Layer
parse line = let (depth:range:_) = words line in (read $ init depth, read range)
