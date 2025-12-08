{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module P7 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Integer
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input7"

parse :: String -> [S.Set Int]
parse = map parseLine . lines

parseLine :: [Char] -> S.Set Int
parseLine = S.fromList . map fst . filter ((/= '.') . snd) . zip [0..]

solve1 :: [S.Set Int] -> Int
solve1 [] = 0
solve1 (start:rows) = countBeams start rows

countBeams :: S.Set Int -> [S.Set Int] -> Int
countBeams beams (splitters:xs) =
    let toSplit = S.intersection beams splitters
        splitCount = length toSplit
        splitBeams =  map (\i -> S.fromList [i-1, i+1]) $ S.toList toSplit 
        remainingBeams = S.difference beams toSplit
        nextBeams = S.unions (remainingBeams : splitBeams)
    in  splitCount + countBeams nextBeams xs
countBeams _ [] = 0

solve2 :: [S.Set Int] -> Integer
solve2 (start:rows) = countPaths (head $ S.toList start) $ foldl unsplitBeams (M.fromList $ map (\i -> (i, 1)) [0..maxWidth]) (reverse rows)
    where maxWidth = (1 +) $ maximum $ S.unions rows
solve2 _ = error "bad input"

countPaths :: Int -> M.Map Int Integer -> Integer
countPaths loc paths = paths M.! loc

unsplitBeams :: M.Map Int Integer -> S.Set Int -> M.Map Int Integer
unsplitBeams possiblePaths splitters = M.fromList $ map (nextPaths splitters possiblePaths) $ M.keys possiblePaths

nextPaths :: S.Set Int -> M.Map Int Integer -> Int -> (Int, Integer)
nextPaths splitters prevPaths location 
    | S.member location splitters = (location, prevPaths M.! (location - 1) + prevPaths M.! (location + 1))
    | otherwise = (location, prevPaths M.! location)