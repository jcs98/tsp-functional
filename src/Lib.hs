module Lib
  ( runMain,
  )
where

import Control.Parallel.Strategies
import Data.List (permutations)
import GeneticUtils
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import System.IO (readFile)
import Types
import Utils

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

geneticMinPathDistance :: [Point] -> Int -> Int -> Int
geneticMinPathDistance [] _ _ = -1
geneticMinPathDistance cities popSize rounds =
  minimum $ map pathDistance finalPop
  where
    population = replicate popSize cities
    finalPop = foldr ($) population (replicate rounds nextGen)

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
    ["-c", ':' : 'n' : n, filename] -> do
      corpus <- readFile filename
      print $ chunkedParallelMinPathDistance (makeCities corpus) (read n)
    ["-g", ':' : 's' : s, ':' : 'r' : r, filename] -> do
      corpus <- readFile filename
      print $ geneticMinPathDistance (makeCities corpus) (read s) (read r)
    _ -> do
      pn <- getProgName
      die $ "Usage: " ++ pn ++ " [-s|-p|-c :nN|-g :sN :rN] <filename>"
