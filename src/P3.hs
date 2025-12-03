module P3 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input3"

parse = lines

solve1 = sum . map joltage

joltage s =
    let a = findFirstNumber s
        b = findSecondNumber a s
    in  read [a,b]

findFirstNumber :: String -> Char
findFirstNumber = maximum . init

findSecondNumber :: Char -> String -> Char
findSecondNumber x = maximum . tail . dropWhile (/= x)

solve2 = sum . map (read . findNextBattery 12)

findNextBattery 0 _ = []
findNextBattery x s =
    let nextBattery = maximum $ take (length s - (x-1)) s
    in  nextBattery : findNextBattery  (x-1) (tail $ dropWhile (/= nextBattery) s) 