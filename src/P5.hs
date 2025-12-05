module P5 (run1, run2, inputLocation) where

import Data.List.Split (splitOn)
import Data.List (sortOn, partition)
import qualified Data.Ord

type Range = (Int, Int)
data Input = Input [Range] [Int]

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input5"

parse :: String -> Input
parse = parseBlocks . splitOn [""] . lines

parseBlocks :: [[String]] -> Input
parseBlocks [ranges, availableIngredients] = Input (map parseRange ranges) (map read availableIngredients)
parseBlocks x = error $ "Expected 2 input blocks, got " ++ show (length x)

parseRange :: String -> Range
parseRange = parseRange' . splitOn "-"

parseRange' :: [String] -> Range
parseRange' [start,end]  = (read start, read end)
parseRange' x = error $ "Malformed range: " ++ show x

solve1 :: Input -> Int
solve1 (Input ranges ingredients) = length $ filter (inRange ranges) ingredients

inRange :: [Range] -> Int -> Bool
inRange ranges ingredient = any (inRange' ingredient) ranges

inRange' :: Int -> Range -> Bool
inRange' ingredient (start, end) = ingredient >= start && ingredient <= end

solve2 :: Input -> Int
solve2 = rangesSize . combineRanges . inputRanges

inputRanges :: Input -> [Range]
inputRanges (Input ranges _) = ranges

combineRanges :: [Range] -> [Range]
combineRanges = combineRanges'  [] . sortOn (Data.Ord.Down . fst)

combineRanges' :: [Range] -> [Range] -> [Range]
combineRanges' acc [] = acc
combineRanges' [] (x:xs) = combineRanges' [x] xs
combineRanges' acc ((start, end):xs) =
    let (overlaps, acc') = partition ((<= end) . fst) acc
        overlapEnd = maximum (end : map snd overlaps)
        combinedOverlaps = (start, overlapEnd)
    in  combineRanges' (combinedOverlaps : acc') xs

rangesSize :: [Range] -> Int
rangesSize = sum . map rangeSize

rangeSize :: Range -> Int
rangeSize (start, end) = end - start + 1