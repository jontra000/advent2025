module P8 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.List (sortOn, partition, sortBy, tails)
import Data.Ord (comparing, Down (Down))

type Coord3 = (Int, Int, Int)

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input8"

parse = map parseLine . lines

parseLine = parseCoord . splitOn ","

parseCoord :: [String] -> Coord3
parseCoord [x,y,z] = (read x, read y, read z)

-- solve1 :: [Coord3] -> Int
solve1 coords = product . take 3 . sortBy (comparing Data.Ord.Down) . groupSizes . take 1000 . sortOn distance $ pairAll coords

pairAll xs = [ (x,y) | (x:ys) <- tails xs, y <- ys]

distance :: (Coord3, Coord3) -> Double
distance ((x1,y1,z1), (x2,y2,z2)) = sqrt $ fromIntegral $ (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2) + (z1 - z2) * (z1 - z2)

groupSizes [] = []
groupSizes ((a,b):xs) =
    let (group', xs') = group (S.fromList [a,b]) xs
    in  length group' : groupSizes xs'

group inGroup toCheck =
    let (toInclude, toCheck') = partition (\(a,b) -> S.member a inGroup || S.member b inGroup) toCheck
    in  if null toInclude
        then (inGroup, toCheck)
        else
            let inGroup' = S.unions (inGroup : map (\(a,b)->S.fromList [a,b]) toInclude)
            in  group inGroup' toCheck'

solve2 coords = result2 . findLastConnection (S.fromList coords) [] . map (\(a,b)-> S.fromList [a,b]) . sortOn distance $ pairAll coords

findLastConnection ungrouped groups (connection:connections) =
    let ungrouped' = S.difference ungrouped connection
        (remainingGroups, matchingGroups) = partition (null . S.intersection connection) groups
        newGroup = S.unions (connection : matchingGroups)
        groups' = newGroup : remainingGroups
    in  if null ungrouped && length groups' == 1
        then connection
        else findLastConnection ungrouped' groups' connections

result2 = product . S.map (\(x,_,_) -> x)