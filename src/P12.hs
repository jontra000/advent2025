module P12 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Lib (Coord, textToCoordMap)

data Region = Region Int Int [Int] -- height width presentsRequired
type Present = S.Set Coord
data Input = Input [Present] [Region]

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input12"

parse :: String -> Input
parse = parseBlocks . reverse . splitOn [""] . lines

parseBlocks :: [[String]] -> Input
parseBlocks (regionBlock:presentBlocks) = Input (parsePresents presentBlocks) (parseRegions regionBlock)
parseBlocks x = error $ "Malformed input: " ++ show x

parsePresents :: [[String]] -> [Present]
parsePresents = map (snd . parsePresent)

parseRegions :: [String] -> [Region]
parseRegions = map (parseRegion . words)

parsePresent :: [String] -> (Int, Present)
parsePresent (header:grid) = (label, parsePresentGrid grid)
    where label = read (init header)
parsePresent x = error $ "Malformed present: " ++ show x

parsePresentGrid :: [String] -> Present
parsePresentGrid = M.keysSet . M.filter (=='#') . textToCoordMap . unlines

parseRegion :: [String] -> Region
parseRegion (dims:reqs) = Region height width (map read reqs)
    where (height, width) = parseDims (splitOn "x" $ init dims)
parseRegion x = error $ "Malformed region: " ++ show x

parseDims :: [String] -> (Int, Int)
parseDims [h, w] = (read h, read w)
parseDims x = error $ "Malformed dimensions: " ++ show x

solve1 :: Input -> Int
solve1 (Input presents regions) = length $ filter (canFit presents) regions

canFit :: [Present] -> Region -> Bool
canFit presents (Region height width reqs) = availableSize > requiredSize' + padding
    where availableSize = height * width
          requiredSize' = requiredSize reqs presents
          padding = sum reqs

requiredSize :: [Int] -> [Present] -> Int
requiredSize reqs = sum . zipWith (*) reqs . map length

solve2 :: b -> Int
solve2 = const 0