module P8 (run1, run2, inputLocation) where

import Data.List.Split (splitOn)
import Data.List (sortOn, sortBy, tails)
import Data.Ord (comparing, Down (Down))
import qualified DisjointSet

type Coord3 = (Int, Int, Int)
type Connection = (Coord3, Coord3)
type State = DisjointSet.DisjointSet Coord3

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
solve1 coords = product . takeLargest 3 . connectPairs disjointSet . takeClosest 1000 $ pairAll coords
    where disjointSet = DisjointSet.fromList coords

pairAll :: [b] -> [(b, b)]
pairAll xs = [ (x,y) | (x:ys) <- tails xs, y <- ys]

takeClosest :: Int -> [Connection] -> [Connection]
takeClosest x = take x . sortOn distance

distance :: Connection -> Double
distance ((x1,y1,z1), (x2,y2,z2)) = sqrt $ fromIntegral $ (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2) + (z1 - z2) * (z1 - z2)

connectPairs :: State -> [Connection] -> State
connectPairs = foldl (\djs' (x, y) -> DisjointSet.union x y djs')

takeLargest :: Int -> State -> [Int]
takeLargest x = take x . sortBy (comparing Data.Ord.Down) . DisjointSet.groupSizes

solve2 :: [Coord3] -> Int
solve2 coords = result2 . findLastConnection disjointSet . sortOn distance $ pairAll coords
    where disjointSet = DisjointSet.fromList coords

findLastConnection :: State -> [Connection] -> Connection
findLastConnection _ [] = error "Groups don't connect"
findLastConnection djs ((x,y):xs)
    | groupCount == 1 = (x,y)
    | otherwise = findLastConnection djs' xs
    where djs' = DisjointSet.union x y djs
          groupCount = length $ DisjointSet.groupSizes djs'

result2 :: Connection -> Int
result2 ((x1, _, _), (x2, _, _)) = x1 * x2