module P3 (run1, run2, inputLocation) where

run1 :: String -> Integer
run1 = solve1 . parse

run2 :: String -> Integer
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input3"

parse :: String -> [String]
parse = lines

solve1 :: [String] -> Integer
solve1 = solve 2

solve2 :: [String] -> Integer
solve2 = solve 12

solve :: Int -> [String] -> Integer
solve count = sum . map (read . findNextBattery count)

findNextBattery :: Ord a => Int -> [a] -> [a]
findNextBattery 0 _ = []
findNextBattery x s =
    let x'= x - 1
        searchLength = length s - x'
        nextBattery = maximum $ take searchLength s
        remainingString = tail $ dropWhile (/= nextBattery) s
    in  nextBattery : findNextBattery x' remainingString