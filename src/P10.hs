{-# LANGUAGE RankNTypes #-}
module P10 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.List (findIndices, transpose, find, sortOn)
import Data.Maybe (mapMaybe)


data Input = Input (S.Set Int) (S.Set (S.Set Int)) [Int] -- target lights, button toggles, joltages

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input10"

parse = map parseLine . lines

parseLine = parseWords . map (tail . init) . words

parseWords xs = Input (parseLights (head xs)) (S.fromList $ map parseButtons (tail (init xs))) (parseJoltages (last xs))

parseLights = S.fromList . map fst . filter ((=='#') . snd) . zip [0..]

parseButtons :: String -> S.Set Int
parseButtons = S.fromList . map read . splitOn ","

parseJoltages = map read . splitOn ","

solve1 = sum . map fewestToggles

fewestToggles (Input target buttons _) = minimum . S.map length . S.filter ((==target) . toggledLights) $ S.powerSet buttons

toggledLights toggles =
    let allVals = S.unions toggles
    in  S.filter isOn allVals
    where isOn x = odd $ length $ S.filter (S.member x) toggles


solve2 = sum . map fewestJoltages

fewestJoltages ::  Input -> Int
fewestJoltages (Input _ buttons target) = assertSolved $ solveSystem system
    where system = makeSystem (S.toList buttons) target
          assertSolved Nothing = error $ "no solution for " ++ show system
          assertSolved (Just x) = x

solveSystem :: [[Int]] -> Maybe Int
solveSystem = solveReduced . dropZeroRows . rref 0

dropZeroRows :: [[Int]] -> [[Int]]
dropZeroRows = filter (any (/= 0))

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

reduceRow i row1 row2 = zipWith (-) (map (* (row1 !! i)) row2) (map (* (row2 !! i)) row1)

factorRow xs = makePositive $ map (`div` scale) xs
    where scale = foldl1 gcd xs
          makePositive ys = if fmap signum (find (/= 0) ys) == Just (-1) then map (* (-1)) ys else ys

solveReduced m =
    bruteForce buttons 0 Nothing (map (const 0) target) target
    where target = map last m
          buttons = reverse $ sortOn (sum . (map abs)) $ filter (any (/= 0)) (init $ transpose m)

makeSystem :: [S.Set Int] -> [Int] -> [[Int]]
makeSystem buttons target = transpose (map buttonToVector buttons ++ [target])
    where maxVal = maximum (S.unions buttons)
          buttonToVector button = map (\i -> if S.member i button then 1 else 0) [0..maxVal]

bruteForce buttons currentCount bestCount state target
    | state == target = case bestCount of 
        Nothing -> Just currentCount
        Just bestCount' -> Just $ min currentCount bestCount'
    | maybe False (< currentCount) bestCount = bestCount
    | null buttons = bestCount
    | otherwise = foldl bruteForce' bestCount pressesToTry
        where maxPresses = sum (map abs target)
              button = head buttons
              pressesToTry = filter (>=0) $ if length (filter (/=0) button) == 1 then [sum (zipWith (\b v -> if b == 0 then 0 else v `div` b) button $ zipWith (-) target state)] else [0..maxPresses]
              bruteForce' bestCount' presses = bruteForce (tail buttons) (currentCount + presses) bestCount' (updateState state button presses) target

updateState state button presses = zipWith (+) state (map (* presses) button)