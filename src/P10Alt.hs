{-# LANGUAGE RankNTypes #-}
module P10Alt (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

type Button = S.Set Int
type LightPattern = Int -- Represent binary field as Int for efficiency
type JoltagePattern = [Int]
data Solution = Solution {buttonCount :: Int, joltagePattern :: JoltagePattern} deriving (Eq, Ord) -- button counts, joltage pattern
data Input = Input LightPattern (S.Set Button) JoltagePattern -- target lights, button toggles, joltages
type PatternSolutionMap = M.Map Int [Solution]

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input10"

parse :: String -> [Input]
parse = map parseLine . lines

parseLine :: String -> Input
parseLine = parseWords . map (tail . init) . words

parseWords :: [String] -> Input
parseWords xs = Input (parseLights xs) (parseButtons xs) (parseJoltages xs)

parseLights :: [String] -> LightPattern
parseLights = toLightPattern . map (=='#') . head

toLightPattern :: [Bool] -> LightPattern
toLightPattern = foldl go 0 . map boolToInt
    where go acc x = x + 2 * acc

boolToInt :: Num a => Bool -> a
boolToInt True = 1
boolToInt False = 0

parseButtons :: [String] -> S.Set Button
parseButtons = S.fromList . map parseButton . tail . init

parseButton :: String -> S.Set Int
parseButton = S.fromList . map read . splitOn ","

parseJoltages :: [String] -> JoltagePattern
parseJoltages = map read . splitOn "," . last

solve1 :: [Input] -> Int
solve1 = sum . map fewestToggles

fewestToggles :: Input -> Int
fewestToggles (Input target buttons _) = fewestToggles' target buttonConfigs
    where registerCount = maximum (S.unions buttons)
          buttonConfigs = makeButtonConfigs registerCount buttons

fewestToggles' :: LightPattern -> PatternSolutionMap -> Int
fewestToggles' target = minimumButtons . solutionsForPattern target

minimumButtons :: [Solution] -> Int
minimumButtons = minimum . map buttonCount

solve2 :: [Input] -> Int
solve2 = sum . map solve2Alt

solve2Alt :: Input -> Int
solve2Alt (Input _ buttons target) = assertSolvable target solution
    where registerCount = length target - 1
          buttonConfigs = makeButtonConfigs registerCount buttons
          solution = solve2Alt' buttonConfigs 1 0 Nothing target

assertSolvable :: JoltagePattern -> Maybe a2 -> a2
assertSolvable _ (Just x) = x
assertSolvable target Nothing = error $ "No solution for " ++ show target

solve2Alt' :: PatternSolutionMap -> Int -> Int -> Maybe Int -> JoltagePattern -> Maybe Int
solve2Alt' buttonConfigs scale currentCount bestCount target
    | all (==0) target = Just $ maybe currentCount (min currentCount) bestCount
    | any (<0) target = bestCount
    | maybe False (<=currentCount) bestCount = bestCount
    | otherwise = foldl go bestCount solutions
    where parities' = parities target
          solutions = solutionsForPattern parities' buttonConfigs
          scale' = scale * 2
          go bestCount' solution = solve2Alt' buttonConfigs scale' currentCount' bestCount' target'
            where currentCount' = currentCount + scale * buttonCount solution
                  target' = updateTarget target solution

solutionsForPattern :: LightPattern -> PatternSolutionMap -> [Solution]
solutionsForPattern pattern = fromMaybe [] . M.lookup pattern

parities :: JoltagePattern -> LightPattern
parities = toLightPattern . map odd

makeButtonConfigs :: Int -> S.Set Button -> PatternSolutionMap
makeButtonConfigs maxRegister = M.fromListWith (++) . S.toList . S.map go . S.powerSet
    where go buttons = (parities', [Solution (length buttons) joltageCounts])
            where joltageCounts = map (joltageCount buttons) [0..maxRegister]
                  parities' = parities joltageCounts

joltageCount :: S.Set Button -> Int -> Int
joltageCount buttons =  length .  (`S.filter` buttons) . S.member

updateTarget :: [Int] -> Solution -> [Int]
updateTarget target = map (`div` 2) . zipWith (-) target . joltagePattern