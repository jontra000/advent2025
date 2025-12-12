module P12 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Lib (Coord, textToCoordMap)

data Region = Region Int Int [Int] -- height width presentsRequired

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input12"

parse = parseBlocks . splitOn [""] . lines

parseBlocks :: [[String]] -> ([S.Set Coord], [Region])
parseBlocks blocks = (map snd $ map parsePresent (init blocks), map parseRegion (map words (last blocks)))

parsePresent :: [String] -> (Int, S.Set Coord)
parsePresent (header:grid) = (read (init header), M.keysSet $ M.filter (=='#') $ textToCoordMap (unlines grid))

parseRegion :: [String] -> Region
parseRegion (dims:reqs) = Region height width (map read reqs)
    where (height, width) = parseDims (splitOn "x" $ init dims)

parseDims [h, w] = (read h, read w)

solve1 (presents, regions) = length $ filter (canFit presents) regions

canFit presents (Region height width reqs) =
    let availableSize = height * width
        requiredSize = sum $ zipWith (*) reqs $ map length presents
        padding = sum reqs
    in  availableSize > requiredSize + padding

solve2 = const 0