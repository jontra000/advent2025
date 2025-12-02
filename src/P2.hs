module P2 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn, chunksOf)

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input2"

parse = map show . concatMap parseLine . splitOn ","

parseLine :: [Char] -> [Int]
parseLine s = 
    let (start:end:_) = map read $ splitOn "-"  s
    in [start..end]

solve1 = sum . map read . filter isInvalid

isInvalid s =
    let size = length s `div` 2
    in  take size s == drop size s

isInvalid2 s = any (isInvalid' s) $ [1..length s `div` 2]

isInvalid' s x =
    let chunks = chunksOf x s
    in  allEqual chunks

allEqual [] = False
allEqual [_] = False
allEqual (x:xs) = all (==x) xs

solve2 = sum . map read . filter isInvalid2