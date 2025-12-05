module P5 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.List (sortOn, partition)

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input5"

parse = parseBlocks . splitOn [""] . lines

parseBlocks :: [[String]] -> ([(Int, Int)], [Int])
parseBlocks [ranges, availableIngredients] = (map parseRange ranges, map read availableIngredients)

parseRange :: String -> (Int, Int)
parseRange = parseRange' . splitOn "-"

parseRange' :: [String] -> (Int, Int)
parseRange' [start,end]  = (read start, read end)

solve1 (ranges, ingredients) = length $ filter (inRange ranges) ingredients

inRange ranges ingredient = any (inRange' ingredient) ranges

inRange' ingredient (start,end) = ingredient >= start && ingredient <= end

solve2 = rangesSize . combineRanges . fst

combineRanges = combineRanges'  [] . reverse . sortOn fst

combineRanges' :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
combineRanges' acc [] = acc
combineRanges' [] (x:xs) = combineRanges' [x] xs
combineRanges' acc ((start, end):xs) =
    let (overlaps, acc') = partition ((<= end) . fst) acc
    in  combineRanges' ((start, maximum (end : map snd overlaps)) : acc') xs

rangesSize = sum . map (\(start, end) -> end - start + 1)