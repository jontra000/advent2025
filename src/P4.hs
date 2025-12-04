module P4 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Lib (textToCoordMap, Coord, neighbours8)

run1 = solve1 . parse
run1 :: String -> Int

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input4"

parse :: String -> S.Set Lib.Coord
parse = M.keysSet . M.filter (== '@') . textToCoordMap

solve1 :: S.Set Lib.Coord -> Int
solve1 = length . movableCoords

movableCoords :: S.Set Lib.Coord -> S.Set Lib.Coord
movableCoords coords = S.filter (canMove coords) coords

canMove :: S.Set Lib.Coord -> Lib.Coord -> Bool
canMove allCoords = (<4) . length . S.intersection allCoords . Lib.neighbours8

solve2 :: S.Set Coord -> Int
solve2 coords=
    let initSize = length coords
        remaining = length $ tryRemove coords
    in  initSize - remaining

tryRemove :: S.Set Coord -> S.Set Coord
tryRemove coords =
    let toRemove = movableCoords coords
    in  if null toRemove
        then coords
        else tryRemove $ S.difference coords toRemove