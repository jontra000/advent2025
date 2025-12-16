module P11 (run1, run2, inputLocation) where

import qualified Data.Map as M
import Data.Maybe (fromMaybe)

type Graph = M.Map String [String]
type Cache = M.Map String Int

run1 :: String -> Int
run1 = solve1 . parse

run2 :: String -> Int
run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input11"

parse :: String -> Graph
parse = M.fromList . map parseLine . lines

parseLine :: String -> (String, [String])
parseLine =  parseConnection . words

parseConnection :: [String] -> (String, [String])
parseConnection (inStr:outStrs) = (init inStr, outStrs)
parseConnection x = error $ "Malformed connection string: " ++ show x

solve1 :: Graph -> Int
solve1 graph = pathsFromTo graph "you" "out"

solve2 :: Graph -> Int
solve2 graph =
    let fftPaths = pathsFromTo graph "svr" "fft"
        dacPaths = pathsFromTo graph "svr" "dac"
        fftdacPaths = pathsFromTo graph "fft" "dac"
        dacfftPaths = pathsFromTo graph "dac" "fft"
        fftoutPaths = pathsFromTo graph "fft" "out"
        dacoutPaths = pathsFromTo graph "dac" "out"
    in  fftPaths * fftdacPaths * dacoutPaths + dacPaths * dacfftPaths * fftoutPaths

pathsFromTo :: Graph -> String -> String -> Int
pathsFromTo graph from = (M.! from) . (\cache -> countPaths graph cache from) . (`M.singleton` 1)

countPaths :: Graph -> Cache -> String -> Cache
countPaths graph visited current =
    case M.lookup current visited of
        Just _ -> visited
        Nothing -> countPaths' current visited graph

countPaths' :: String -> Cache -> Graph -> Cache
countPaths' current visited graph = M.insert current paths visited'
    where toVisit = connections current graph
          visited' = foldl (countPaths graph) visited toVisit
          paths = sumPaths visited' toVisit

connections :: String -> Graph -> [String]
connections current = fromMaybe [] . M.lookup current

sumPaths :: Cache -> [String] -> Int
sumPaths cache = sum . map (cache M.!)