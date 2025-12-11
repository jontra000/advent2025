module P11 (run1, run2, inputLocation) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (delete)
import Data.Maybe (fromJust, fromMaybe)

run1 = solve1 . parse

run2 = solve2 . parse

inputLocation :: String
inputLocation = "inputs/input11"

parse = M.fromList . map parseLine . lines

parseLine =  parseConnection . words

parseConnection :: [String] -> (String, [String])
parseConnection (inStr:outStrs) = (init inStr, outStrs)

solve1 = countPaths "you"

countPaths "out" _ = 1
countPaths x graph = sum (map (\x' -> countPaths x' graph) (graph M.! x ))

countPaths2 [] "out" _ = 1
countPaths2 _ "out" _ = 0
countPaths2 toVisit x graph = sum (map (\x' -> countPaths2 toVisit' x' graph) (graph M.! x ))
    where toVisit' = delete x toVisit

countPaths3 current target graph
    | current == target = 1
    | otherwise = sum (map (\x' -> countPaths3 x' target graph) (graph M.! current ))

-- solve2 = countPaths4 "svr" S.empty
--     -- let fftPaths = countPaths3 "svr" "fft" graph 
--     --     dacPaths = countPaths3 "svr" "dac" graph 
--     --     fftdacPaths = countPaths3 "fft" "dac" graph 
--     --     dacfftPaths = countPaths3 "dac" "fft" graph 
--     --     fftoutPaths = countPaths3 "fft" "out" graph 
--     --     dacoutPaths = countPaths3 "dac" "out" graph 
--     -- in  fftPaths * fftdacPaths * dacoutPaths + dacPaths * dacfftPaths * fftoutPaths

-- countPaths4 "out" visited _ =
--     if S.member "fft" visited && S.member "dac" visited then 1 else 0
-- countPaths4 current visited graph
--     | S.member current visited = 0
--     | otherwise = sum (map (\x' -> countPaths4 x' visited' graph) (graph M.! current ))
--     where visited' = S.insert current visited


solve2 graph = 
    let fftPaths = pathsFromTo "svr" "fft" graph 
        dacPaths = pathsFromTo "svr" "dac" graph 
        fftdacPaths = pathsFromTo "fft" "dac" graph 
        dacfftPaths = pathsFromTo "dac" "fft" graph 
        fftoutPaths = pathsFromTo "fft" "out" graph 
        dacoutPaths = pathsFromTo "dac" "out" graph 
    in  fftPaths * fftdacPaths * dacoutPaths + dacPaths * dacfftPaths * fftoutPaths

pathsFromTo from to graph = fromJust $  M.lookup from (countPaths4 from (M.singleton to 1) graph)

countPaths4 :: String -> M.Map String Int -> M.Map String [String] -> M.Map String Int
countPaths4 current visited graph =
    case M.lookup current visited of
        Just _ -> visited
        Nothing ->
            let toVisit = fromMaybe [] $ M.lookup current graph 
                visited' = foldl (\state next -> countPaths4 next state graph) visited toVisit
                paths = sum $ map (visited' M.!) toVisit
            in  M.insert current paths visited'
