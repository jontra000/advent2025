module P4 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Lib (textToCoordMap)
import Data.List (inits)

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input4"

parse = M.keysSet . M.filter (== '@') . textToCoordMap

solve1 coords = length $ S.filter (canMove coords) coords

canMove allCoords x =
    let neighbours = S.filter (isNeighbour x) allCoords
    in  length neighbours < 5

isNeighbour (x1,y1) (x2,y2) =
    abs (x1 - x2) <= 1 && abs (y1 - y2) <= 1

solve2 coords=
    let initSize = length coords
        remaining = length $ tryRemove coords
    in  initSize - remaining

tryRemove coords =
    let toRemove = S.filter (canMove coords) coords
    in  if null toRemove
        then coords
        else tryRemove $ S.difference coords toRemove