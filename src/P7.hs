{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use map with tuple-section" #-}
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

parseLine :: String -> S.Set Int
parseLine = S.fromList . indicesWhere isSplitterLoc

isSplitterLoc :: Char -> Bool
isSplitterLoc = (/= '.')

indicesWhere :: (a -> Bool) -> [a] -> [Int]
indicesWhere predicate = map fst . filter (predicate . snd) . zip [0..]

solve1 :: [S.Set Int] -> Int
solve1 [] = 0
solve1 (start:rows) = countSplits rows $ beamPath start rows

nextBeam :: S.Set Int -> Int -> S.Set Int
nextBeam splitters x
    | S.member x splitters = S.fromList [x-1, x+1]
    | otherwise = S.singleton x

beamPath :: S.Set Int -> [S.Set Int] -> [S.Set Int]
beamPath = scanl stepBeams

stepBeams :: S.Set Int -> S.Set Int -> S.Set Int
stepBeams beams splitters = S.unions (S.map (nextBeam splitters) beams)

countSplits :: [S.Set Int] -> [S.Set Int] -> Int
countSplits splitters = sum . map length . zipWith S.intersection splitters

solve2 :: [S.Set Int] -> Integer
solve2 (start:rows) = countPaths startLoc . foldl unsplitBeams pathsInit $ reverse rows
    where maxWidth = 2 * length rows
          startLoc = head $ S.toList start
          pathsInit = M.fromList $ zip [0..maxWidth] (repeat 1)
solve2 _ = error "bad input"

countPaths :: Int -> M.Map Int Integer -> Integer
countPaths loc paths = paths M.! loc

unsplitBeams :: M.Map Int Integer -> S.Set Int -> M.Map Int Integer
unsplitBeams possiblePaths splitters = M.mapWithKey (\k _ -> nextPaths splitters possiblePaths k) possiblePaths

nextPaths :: S.Set Int -> M.Map Int Integer -> Int -> Integer
nextPaths splitters prevPaths location
    | S.member location splitters = prevPaths M.! (location - 1) + prevPaths M.! (location + 1)
    | otherwise = prevPaths M.! location