{-# LANGUAGE RankNTypes #-}
module P10 (run1, run2, inputLocation) where

import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.List (transpose, find, partition, maximumBy)
import Data.Function (on)

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
    bruteForce target fixedButtons' freeButtons 0 Nothing initState
    where target = map last m
          initState = map (const 0) target
          buttons = filter isNonZeroRow (init $ transpose m)
          (fixedButtons, freeButtons) = partition isFixedButton buttons
          fixedButtons' = compressFixedButtons fixedButtons

isFixedButton :: [Int] -> Bool
isFixedButton = (==1) . length . filter (/= 0)

compressFixedButtons :: [[Int]] -> [Int]
compressFixedButtons = map (maximumBy (compare `on` abs)) . transpose

makeMatrix :: [S.Set Int] -> [Int] -> [[Int]]
makeMatrix buttons = transpose . (map (makeVector maxVal) buttons ++) . (: [])
    where maxVal = maximum (S.unions buttons)

makeVector :: Int -> S.Set Int -> [Int]
makeVector maxVal button = map (\i -> if S.member i button then 1 else 0) [0..maxVal]

bruteForce :: [Int] -> [Int] -> [[Int]] -> Int -> Maybe Int -> [Int] -> Maybe Int
bruteForce target solvedButtons = go
    where maxPresses = maximum (map abs target)
          go freeButtons currentCount bestCount state
            | maybe False (<= currentCount) bestCount = bestCount
            | state == target = Just $ maybe currentCount (min currentCount) bestCount
            | null freeButtons = applySolvedButtons target solvedButtons state bestCount currentCount
            | otherwise = foldl go' bestCount pressesToTry'
                where button = head freeButtons
                      pressesToTry' = [0..maxPresses]
                      go' bestCount' presses = go (tail freeButtons) (currentCount + presses) bestCount' (updateState state button presses)

applySolvedButtons :: [Int] -> [Int] -> [Int] -> Maybe Int -> Int -> Maybe Int
applySolvedButtons target solvedButtons state bestCount currentCount = minMaybe bestCount (fmap (+currentCount) newCount)
    where newCount = trySolve solvedButtons target state

trySolve :: [Int] -> [Int] -> [Int] -> Maybe Int
trySolve solvedButtons target state =
    let delta = zipWith (-) target state
        solution = filter ((/= 0) . fst) $ zip delta solvedButtons
        solution' = map (uncurry div) solution
    in  if any (<0) solution' || any ((/= 0) . uncurry mod) solution
        then Nothing
        else Just $ sum solution'

updateState :: [Int] -> [Int] -> Int -> [Int]
updateState state button presses = zipWith (+) state (map (* presses) button)

minMaybe :: Ord a => Maybe a -> Maybe a -> Maybe a
minMaybe Nothing Nothing = Nothing
minMaybe (Just x) (Just y) = Just (min x y)
minMaybe x Nothing = x
minMaybe _ y = y