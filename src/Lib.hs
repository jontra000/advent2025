{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use bimap" #-}
module Lib (memoize, textToCoordMap, Coord, addCoords, subtractCoords, mulCoord, Direction(..), move, rotateRight, rotateLeft, reverseDir, neighbours4, neighbours8) where

import qualified Data.Map as M
import qualified Data.Set as S

type Coord = (Int, Int)
data Direction = DirUp | DirDown | DirLeft | DirRight deriving (Eq, Ord, Show)

textToCoordMap :: String -> M.Map Coord Char
textToCoordMap = M.fromList . concat . zipWith (\y line -> zipWith(\x c -> ((x,y), c)) [0..] line) [0..] . lines

addCoords :: Coord -> Coord -> Coord
addCoords (x1, y1) (x2, y2) = (x1+x2, y1+y2)

subtractCoords :: Coord -> Coord -> Coord
subtractCoords (x1,y1) (x2,y2) = (x1-x2, y1-y2)

mulCoord :: Int -> Coord -> Coord
mulCoord n (x,y) = (n*x,n*y)

move :: Direction -> Coord -> Coord
move DirUp (x,y) = (x, y-1)
move DirDown (x,y) = (x,y+1)
move DirLeft (x,y) = (x-1,y)
move DirRight (x,y) = (x+1,y)

neighbours4 :: Coord -> S.Set Coord
neighbours4 (x,y) = S.fromList [(x, y-1), (x, y+1), (x-1,y), (x+1,y)]

neighbours8 :: Coord -> S.Set Coord
neighbours8 (x,y) = S.fromList . map (\(x', y') -> (x+x',y+y')) $ [(x', y') | x' <- [-1,0,1], y' <- [-1,0,1], (x',y') /= (0,0)]

rotateRight :: Direction -> Direction
rotateRight DirUp = DirRight
rotateRight DirRight = DirDown
rotateRight DirDown = DirLeft
rotateRight DirLeft = DirUp

rotateLeft :: Direction -> Direction
rotateLeft DirUp = DirLeft
rotateLeft DirRight = DirUp
rotateLeft DirDown = DirRight
rotateLeft DirLeft = DirDown

reverseDir :: Direction -> Direction
reverseDir DirUp = DirDown
reverseDir DirLeft = DirRight
reverseDir DirRight = DirLeft
reverseDir DirDown = DirUp

memoize :: Ord a => M.Map a b -> a -> (b, M.Map a b) -> (b, M.Map a b)
memoize cache key f = 
    case M.lookup key cache of
        Just x -> (x, cache)
        Nothing ->
            let (result, cache') = f
            in  (result, M.insert key result cache')