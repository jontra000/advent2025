module P9 (run1, run2, inputLocation) where

import Data.List.Split (splitOn)
import Lib (Coord)
import Data.List (tails, nub)

type Edge = (Coord, Coord)

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input9"

parse :: String -> [Coord]
parse = map parseLine . lines

parseLine :: String -> Coord
parseLine = parseCoord . splitOn ","

parseCoord :: [String] -> Coord
parseCoord [x,y] = (read x, read y)
parseCoord x = error $ "Malformed coordinate: " ++ show x

allPairs :: [Coord] -> [Edge]
allPairs xs = [ (x,y) | (x:ys) <- tails xs, y <- ys]

solve1 :: [Coord] -> Int
solve1 = maximum . map rectSize . allPairs

rectSize :: (Coord, Coord) -> Int
rectSize ((x1, y1), (x2, y2)) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

solve2 :: [Coord] -> Int
solve2 redTiles
    | not (validateAssumptions edges') = error "bad edges"
    | otherwise = maximum . map rectSize . filter (squareFits redTiles) $ allPairs redTiles
    where edges' = edges redTiles

edges :: [Coord] -> [Edge]
edges [] = []
edges (x:xs) = zip (x:xs) (xs ++ [x])

validateAssumptions :: [Edge] -> Bool
validateAssumptions edges' = all isRectilinear edges' && noAlignedEdges edges'

isRectilinear :: Edge -> Bool
isRectilinear ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

noAlignedEdges :: [Edge] -> Bool
noAlignedEdges edges' =
    let xEdges = map (fst . fst) $ filter isVertical edges'
        yEdges = map (fst . snd) $ filter isHorizontal edges'
    in  length (nub xEdges) == length xEdges && length (nub yEdges) == length yEdges

isVertical :: Edge -> Bool
isVertical (c1, c2) = fst c1 == fst c2

isHorizontal :: Edge -> Bool
isHorizontal (c1, c2) = snd c1 == snd c2

squareFits :: [Coord] -> Edge -> Bool
squareFits points (square1, square2) = not (any (intersectsSquare square1 square2) edges') && centreIsInShape square1 square2 edges'
    where edges' = edges points

intersectsSquare :: Coord -> Coord -> Edge -> Bool
intersectsSquare (squareX1, squareY1) (squareX2, squareY2) ((edgeX1, edgeY1), (edgeX2, edgeY2))
    | edgeX1 == edgeX2 = minX < edgeX1 && edgeX1 < maxX && (minYEdge < maxY && maxYEdge > minY)
    | otherwise =        minY < edgeY1 && edgeY1 < maxY && (minXEdge < maxX && maxXEdge > minX)
        where minY = min squareY1 squareY2
              maxY = max squareY1 squareY2
              minX = min squareX1 squareX2
              maxX = max squareX1 squareX2
              minXEdge = min edgeX1 edgeX2
              maxXEdge = max edgeX1 edgeX2
              minYEdge = min edgeY1 edgeY2
              maxYEdge = max edgeY1 edgeY2

centreIsInShape :: Coord -> Coord -> [Edge] -> Bool
centreIsInShape (x1, y1) (x2, y2) = isCoordInShape ((x1 + x2) `div` 2) ((y1 + y2) `div` 2)

isCoordInShape :: Int -> Int -> [Edge] -> Bool
isCoordInShape x y edges' = isOnEdge x y edges' || odd (length outerEdges)
    where outerEdges = filter (isOuterEdge (x,y)) edges'

isOuterEdge :: Coord -> Edge -> Bool
isOuterEdge (x,y) ((xE1, yE1), (_, yE2))
    | yE1 == yE2 = False
    | otherwise = xE1 > x && max yE1 yE2 > y && min yE1 yE2 <= y

isOnEdge :: Int -> Int -> [Edge] -> Bool
isOnEdge x y = any (isOnEdge' x y)

isOnEdge' :: Int -> Int -> Edge -> Bool
isOnEdge' x y ((x1, y1), (x2, y2))
    | x == x1 && x == x2 = y >= min y1 y2 && y <= max y1 y2
    | y == y1 && y == y2 = x >= min x1 x2 && x <= max x1 x2
    | otherwise = False