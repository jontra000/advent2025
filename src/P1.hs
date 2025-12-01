module P1 (run1, run2, inputLocation) where

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input1"

parse :: String -> [Int]
parse = map parseLine . lines

parseLine :: String -> Int
parseLine ('R':xs) = read xs
parseLine ('L':xs) = -(read xs)
parseLine x = error $ "Bad input: " ++ x

solve1 :: [Int] -> Int
solve1 = length . filter (== 0) . map (`mod` 100) . scanl (+) 50

solve2 :: [Int] -> Int
solve2 = countZeros 50

countZeros :: Integral t => t -> [t] -> t
countZeros _ [] = 0
countZeros start (delta:xs) =
    let next = (start + delta)
        zeros = abs (next `quot` 100) + if signum start == -(signum next) || next == 0 then 1 else 0
    in  zeros + countZeros (next  `mod` 100) xs