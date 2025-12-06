module P6 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

run1 = solve1 . parse

run2 = solve2 . parse2

inputLocation :: String
inputLocation = "inputs/input6"

parse = transpose . map parseLine . lines

parseLine = words

solve1 = sum . map solveProblem

solveProblem :: [String] -> Integer
solveProblem [] = 0
solveProblem xs =
    let vars = map read $ init xs
        operator = parseOperator $ last xs
    in  foldl1 operator vars

parseOperator "+" a b = a + b
parseOperator "*" a b = a * b

parse2 = map parseCephalopod . splitOn ["     "] . transpose . lines

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseCephalopod :: [String] -> Integer
parseCephalopod [] = 0
parseCephalopod xs =
    let operator = parseOperator [(last (head xs))]
        vars = map read . filter (not  . null) . map (trim . init) $ xs
    in  foldl1 operator vars

solve2 = sum