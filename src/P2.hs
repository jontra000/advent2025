module P2 (run1, run2, inputLocation) where

import Data.List.Split (splitOn, chunksOf)

run1 :: [Char] -> Integer
run1 = solve1 . parse

run2 :: [Char] -> Integer
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input2"

parse :: [Char] -> [String]
parse = map show . concatMap (parseRange . splitOn "-") . splitOn ","

parseRange :: [[Char]] -> [Int]
parseRange [start,end] = [read start..read end]
parseRange x = error $ "Badly formatted range: " ++ show x

solve1 :: [String] -> Integer
solve1 = sum . map read . filter isInvalid1

isInvalid1 :: String -> Bool
isInvalid1 s = 
    let size = length s `div` 2
    in  take size s == drop size s

isInvalid2 :: Eq a => [a] -> Bool
isInvalid2 s = any (isInvalid s) [1..length s `div` 2]

isInvalid :: Eq e => [e] -> Int -> Bool
isInvalid s x =
    let chunks = chunksOf x s
    in  allEqual chunks

allEqual :: Eq a => [a] -> Bool
allEqual [] = False
allEqual [_] = False
allEqual (x:xs) = all (==x) xs

solve2 :: [String] -> Integer
solve2 = sum . map read . filter isInvalid2