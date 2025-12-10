module P9 (run1, run2, inputLocation) where

import Data.List.Split (splitOn)
import Lib (Coord)
import Data.List (tails, nub)

type Edge = (Coord, Coord)

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input9"

parse = map parseLine . lines

parseLine = parseCoord . splitOn ","

parseCoord :: [String] -> Coord
parseCoord [x,y] = (read x, read y)

pairAll :: [b] -> [(b, b)]
pairAll xs = [ (x,y) | (x:ys) <- tails xs, y <- ys]

solve1 = maximum . map rectSize . pairAll

rectSize ((x1, y1), (x2, y2)) = (abs (x1 - x2) + 1) * (abs (y1 - y2) + 1)

solve2 redTiles
    | not (validateEdges edges) = error "bad edges"
    | otherwise = maximum . map rectSize . filter (squareFits redTiles) $ pairAll redTiles
    where edges = allEdges redTiles

allEdges [] = []
allEdges (x:xs) = zip (x:xs) (xs ++ [x])

validateEdges edges = all isRectilinear edges && noAlignedEdges edges

isRectilinear ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

noAlignedEdges edges =
    let xEdges = map (fst . fst) $ filter (\((x1, y1), (x2, y2)) -> x1 == x2) edges
        yEdges = map (fst . snd) $ filter (\((x1, y1), (x2, y2)) -> y1 == y2) edges
    in  length (nub xEdges) == length xEdges && length (nub yEdges) == length yEdges

squareFits points (square1@(x1, y1), square2@(x2, y2)) = not (any (intersectsSquare square1 square2) edges) && (centreIsInShape square1 square2 edges)
    where edges = allEdges points
    
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

centreIsInShape (x1, y1) (x2, y2) = isCoordInShape ((x1 + x2) `div` 2) ((y1 + y2) `div` 2)

isCoordInShape :: Int -> Int -> [Edge] -> Bool
isCoordInShape x y edges = isOnEdge x y edges || odd (length outerEdges)
    where outerEdges = filter (isOuterEdge (x,y)) edges

isOuterEdge (x,y) ((xE1, yE1), (_, yE2))
    | yE1 == yE2 = False
    | otherwise = xE1 > x && max yE1 yE2 > y && min yE1 yE2 <= y

isOnEdge x y = any (isOnEdge' x y)

isOnEdge' x y ((x1, y1), (x2, y2))
    | x == x1 && x == x2 = y >= min y1 y2 && y <= max y1 y2
    | y == y1 && y == y2 = x >= min x1 x2 && x <= max x1 x2
    | otherwise = False