module P6 (run1, run2, inputLocation) where

import Data.List ( transpose, dropWhileEnd )
import Data.List.Split (splitWhen)
import Data.Char (isSpace)

type Operator = (Integer -> Integer -> Integer)
data Problem = Problem Operator [Integer] -- operator, values
type Input = [Problem]

run1 :: String -> Integer
run1 = solve . parse parseValues1

run2 :: String -> Integer
run2 = solve . parse parseValues2

inputLocation :: String
inputLocation = "inputs/input6"

parse :: ([String] -> [[Integer]]) -> String -> Input
parse parseValues = parseProblems parseValues . lines

parseProblems :: ([String] -> [[Integer]]) -> [String] -> Input
parseProblems _ [] = []
parseProblems parseValues xs = zipWith Problem (parseOperators (last xs)) (parseValues (init xs))

parseOperators :: String -> [Operator]
parseOperators = map parseOperator . words

parseValues1 :: [String] -> [[Integer]]
parseValues1 = transpose . map (map read . words)

parseValues2 :: [String] -> [[Integer]]
parseValues2 = map (map (read . trim)) . chunkProblems

chunkProblems :: [String] -> [[String]]
chunkProblems = splitWhen (null . trim) . transpose

solve :: Input -> Integer
solve = sum . map solveProblem

solveProblem :: Problem -> Integer
solveProblem (Problem operator xs) = foldl1 operator xs

parseOperator :: Num a => String -> a -> a -> a
parseOperator "+" a b = a + b
parseOperator "*" a b = a * b
parseOperator x _  _ = error $ "Bad operator: " ++ show x

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace