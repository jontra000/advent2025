{-# LANGUAGE RankNTypes #-}
module P10 (run1, run2, inputLocation) where

import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.List (transpose, find, sortOn)
import qualified Data.Ord

type Button = S.Set Int
type LightPattern = S.Set Int
data Input = Input LightPattern (S.Set Button) [Int] -- target lights, button toggles, joltages

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

parseLights :: [String] -> S.Set Int
parseLights = S.fromList . map fst . filter ((=='#') . snd) . zip [0..] . head

parseButtons :: [String] -> S.Set Button
parseButtons = S.fromList . map parseButton . tail . init

parseButton :: String -> S.Set Int
parseButton = S.fromList . map read . splitOn ","

parseJoltages :: [String] -> [Int]
parseJoltages = map read . splitOn "," . last

solve1 :: [Input] -> Int
solve1 = sum . map fewestToggles

fewestToggles :: Input -> Int
fewestToggles (Input target buttons _) = minimumButtons . S.filter (matchesTarget target) $ S.powerSet buttons

minimumButtons :: S.Set (S.Set Button) -> Int
minimumButtons = minimum . S.map length

matchesTarget :: LightPattern -> S.Set Button -> Bool
matchesTarget target = (==target) . toggledLights

toggledLights :: S.Set Button -> LightPattern
toggledLights toggles = S.filter isOn allVals
    where isOn x = odd $ length $ S.filter (S.member x) toggles
          allVals = S.unions toggles

solve2 :: [Input] -> Int
solve2 = sum . map fewestJoltages

fewestJoltages ::  Input -> Int
fewestJoltages (Input _ buttons target) = assertSolved $ solveSystem system
    where system = makeMatrix (S.toList buttons) target
          assertSolved Nothing = error $ "no solution for " ++ show system
          assertSolved (Just x) = x

solveSystem :: [[Int]] -> Maybe Int
solveSystem = solveReduced . dropZeroRows . rref 0

dropZeroRows :: [[Int]] -> [[Int]]
dropZeroRows = filter isNonZeroRow

isNonZeroRow :: [Int] -> Bool
isNonZeroRow = any (/=0)

rref :: Int -> [[Int]] -> [[Int]]
rref i m
    | i >= length (head m) = m
    | otherwise =
        case find (findNextRow i) m of
            Nothing -> rref (i+1) m
            Just nextRow ->
                let m' = filter (/= nextRow) m
                    m'' = nextRow : map (reduceRow i nextRow) m'
                in  rref (i+1) $ map factorRow m''

findNextRow :: Int -> [Int] -> Bool
findNextRow i = (== i) . length . takeWhile (==0)

reduceRow :: Int -> [Int] -> [Int] -> [Int]
reduceRow i row1 row2 = zipWith (-) (scaleRow (row1 !! i) row2) (scaleRow (row2 !! i) row1)

scaleRow :: Int -> [Int] -> [Int]
scaleRow k = map (* k)

factorRow :: [Int] -> [Int]
factorRow xs = makePositive $ map (`div` scale) xs
    where scale = foldl1 gcd xs
          makePositive ys = if fmap signum (find (/= 0) ys) == Just (-1) then scaleRow (-1) ys else ys

solveReduced :: [[Int]] -> Maybe Int
solveReduced m =
    bruteForce buttons 0 Nothing initState target
    where target = map last m
          initState = map (const 0) target
          buttons = sortOn (Data.Ord.Down . sum . map abs) (filter isNonZeroRow (init $ transpose m))

makeMatrix :: [S.Set Int] -> [Int] -> [[Int]]
makeMatrix buttons = transpose . (map (makeVector maxVal) buttons ++) . (: [])
    where maxVal = maximum (S.unions buttons)

makeVector :: Int -> S.Set Int -> [Int]
makeVector maxVal button = map (\i -> if S.member i button then 1 else 0) [0..maxVal]

bruteForce :: [[Int]] -> Int -> Maybe Int -> [Int] -> [Int] -> Maybe Int
bruteForce buttons currentCount bestCount state target
    | state == target = Just $ maybe currentCount (min currentCount) bestCount 
    | maybe False (<= currentCount) bestCount = bestCount
    | null buttons = bestCount
    | otherwise = foldl bruteForce' bestCount pressesToTry'
        where button = head buttons
              pressesToTry' = pressesToTry button target state
              bruteForce' bestCount' presses = bruteForce (tail buttons) (currentCount + presses) bestCount' (updateState state button presses) target

pressesToTry :: [Int] -> [Int] -> [Int] -> [Int]
pressesToTry button target state =
    filter (>=0) $ if isSingleValue then [exactSolution] else [0..maxPresses]
    where maxPresses = sum (map abs target)
          isSingleValue = length (filter (/=0) button) == 1
          exactSolution = sum (zipWith (\b v -> if b == 0 then 0 else v `div` b) button $ zipWith (-) target state)

updateState :: [Int] -> [Int] -> Int -> [Int]
updateState state button presses = zipWith (+) state (map (* presses) button)