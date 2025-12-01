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
solve2 = passwordMethod2 50

passwordMethod2 :: Int -> [Int] -> Int
passwordMethod2 _ [] = 0
passwordMethod2 start (delta:xs) =
    let next = (start + delta) `mod` 100
        zeros = zeroCount start delta
    in  zeros + passwordMethod2 next xs

zeroCount :: Int -> Int -> Int
zeroCount start delta =
    let start' = if delta < 0 && start > 0 then 100 - start else start
        end = start' + abs delta
    in  end `div` 100