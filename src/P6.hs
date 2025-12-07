module P6 (run1, run2, inputLocation) where

import Data.List ( transpose, dropWhileEnd )
import Data.List.Split (splitOn)
import Data.Char (isSpace)

run1 :: String -> Integer
run1 = solve1 . parse

run2 :: String -> Integer
run2 = solve2 . parse2

inputLocation :: String
inputLocation = "inputs/input6"

parse :: String -> [[String]]
parse = transpose . map parseLine . lines

parseLine :: String -> [String]
parseLine = words

solve1 :: [[String]] -> Integer
solve1 = sum . map solveProblem

solveProblem :: [String] -> Integer
solveProblem [] = 0
solveProblem xs =
    let vars = map read $ init xs
        operator = parseOperator $ last xs
    in  foldl1 operator vars

parseOperator :: Num a => String -> a -> a -> a
parseOperator "+" a b = a + b
parseOperator "*" a b = a * b
parseOperator x _  _ = error $ "Bad operator: " ++ show x

parse2 :: String -> [Integer]
parse2 = map parseCephalopod . splitOn ["     "] . transpose . lines

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

parseCephalopod :: [String] -> Integer
parseCephalopod [] = 0
parseCephalopod xs =
    let operator = parseOperator [last (head xs)]
        vars = map read . filter (not  . null) . map (trim . init) $ xs
    in  foldl1 operator vars

solve2 :: [Integer] -> Integer
solve2 = sum