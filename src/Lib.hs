module Lib
  ( runMain,
  )
where

import Control.Parallel.Strategies
import Data.List (permutations)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import System.IO (readFile)

type Point = (Int, Int)

squaredDistance :: Point -> Point -> Int
squaredDistance (x1, y1) (x2, y2) = ((x2 - x1) ^ 2) + ((y2 - y1) ^ 2)

distance :: Point -> Point -> Int
distance a b = floor . sqrt . fromIntegral $ squaredDistance a b

makeCities :: String -> [Point]
makeCities corpus = makePairs $ map read $ words corpus
  where
    makePairs [] = []
    makePairs [p] = [(p, p)] -- replicate last coordinate if odd numbers
    makePairs (p : q : r) = (p, q) : makePairs r

pathDistance :: [Point] -> Int
pathDistance cities = sum $ zipWith distance path (tail path)
  where
    path = last cities : cities

-- Consider all permutations while keeping starting point fixed
minPathDistance :: [Point] -> Int
minPathDistance [] = -1
minPathDistance (c : cities) =
  minimum $
    map (pathDistance . (c :)) $
      permutations cities

parallelMinPathDistance :: [Point] -> Int
parallelMinPathDistance [] = -1
parallelMinPathDistance (c : cities) =
  minimum $
    parMap rdeepseq (pathDistance . (c :)) $
      permutations cities

chunkedParallelMinPathDistance :: [Point] -> Int -> Int
chunkedParallelMinPathDistance [] _ = -1
chunkedParallelMinPathDistance (c : cities) chunkSize =
  minimum $
    withStrategy (parListChunk chunkSize rdeepseq)
      . map (pathDistance . (c :))
      $ permutations cities

runMain :: IO ()
runMain = do
  args <- getArgs
  case args of
    ["-s", filename] -> do
      corpus <- readFile filename
      print $ minPathDistance $ makeCities corpus
    ["-p", filename] -> do
      corpus <- readFile filename
      print $ parallelMinPathDistance $ makeCities corpus
    ['-' : 'c' : n, filename] -> do
      corpus <- readFile filename
      print $ chunkedParallelMinPathDistance (makeCities corpus) (read n)
    _ -> do
      pn <- getProgName
      die $ "Usage: " ++ pn ++ " [-s|-p|-cN] <filename>"
