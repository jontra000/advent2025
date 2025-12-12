module P8 (run1, run2, inputLocation) where

import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.List (sortOn, partition, sortBy, tails)
import Data.Ord (comparing, Down (Down))

type Coord3 = (Int, Int, Int)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input8"

parse :: String -> [Coord3]
parse = map parseLine . lines

parseLine :: String -> Coord3
parseLine = parseCoord . splitOn ","

parseCoord :: [String] -> Coord3
parseCoord [x,y,z] = (read x, read y, read z)
parseCoord x = error $ "Malformed coordinate: " ++ show x

solve1 :: [Coord3] -> Int
solve1 = product . takeSmallest 3 . groupPairs . takeClosest 1000 . pairAll

pairAll :: [b] -> [(b, b)]
pairAll xs = [ (x,y) | (x:ys) <- tails xs, y <- ys]

takeClosest :: Int -> [(Coord3, Coord3)] -> [(Coord3, Coord3)]
takeClosest x = take x . sortOn distance

distance :: (Coord3, Coord3) -> Double
distance ((x1,y1,z1), (x2,y2,z2)) = sqrt $ fromIntegral $ (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2) + (z1 - z2) * (z1 - z2)

groupPairs :: Ord a => [(a, a)] -> [S.Set a]
groupPairs [] = []
groupPairs (pair:xs) =
    let (group', xs') = group (pairToSet pair) xs
    in  group' : groupPairs xs'

pairToSet :: Ord a => (a, a) -> S.Set a
pairToSet (a, b) = S.fromList [a, b]

group :: Ord a => S.Set a -> [(a, a)] -> (S.Set a, [(a, a)])
group inGroup toCheck 
    | null toInclude = (inGroup, toCheck)
    | otherwise = group inGroup' toCheck'
    where pairInGroup (a, b) = S.member a inGroup || S.member b inGroup
          (toInclude, toCheck') = partition pairInGroup toCheck
          newItemSets = map pairToSet toInclude
          inGroup' = S.unions (inGroup : newItemSets)

takeSmallest :: Foldable t => Int -> [t a] -> [Int]
takeSmallest x = take x . sortBy (comparing Data.Ord.Down) . map length

solve2 :: [Coord3] -> Int
solve2 coords = result2 . findLastConnection (S.fromList coords) [] . map pairToSet . sortOn distance $ pairAll coords

findLastConnection :: Ord a => S.Set a -> [S.Set a] -> [S.Set a] -> S.Set a
findLastConnection ungrouped groups (connection:connections)
    | null ungrouped && length groups' == 1 = connection
    | otherwise = findLastConnection ungrouped' groups' connections
    where ungrouped' = S.difference ungrouped connection
          (remainingGroups, matchingGroups) = partition (null . S.intersection connection) groups
          newGroup = S.unions (connection : matchingGroups)
          groups' = newGroup : remainingGroups
findLastConnection _ _ [] = error "Groups not connected"

result2 :: S.Set (Int, b, c) -> Int
result2 = product . S.map (\(x, _, _) -> x)