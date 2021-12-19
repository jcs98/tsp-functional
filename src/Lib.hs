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
    parMap rseq (pathDistance . (c :)) $
      permutations cities

chunkedParallelMinPathDistance :: [Point] -> Int -> Int
chunkedParallelMinPathDistance [] _ = -1
chunkedParallelMinPathDistance (c : cities) chunkSize =
  minimum $
    withStrategy (parListChunk chunkSize rdeepseq)
      . map (pathDistance . (c :))
      $ permutations cities

batchMinPathDistance :: [[Point]] -> [Int]
batchMinPathDistance = map minPathDistance

batchParallelMinPathDistance :: [[Point]] -> [Int]
batchParallelMinPathDistance = parMap rseq minPathDistance

geneticMinPathDistance :: Int -> Int -> [Point] -> Int
geneticMinPathDistance _ _ [] = -1
geneticMinPathDistance populationSize generations cities =
  minimum $ map pathDistance finalPop
  where
    population = replicate populationSize cities
    randomList = randomListInRange 0 (length cities - 1)
    finalPop =
      fst $
        foldr
          (\f (p, r) -> (f p r, tail r))
          (population, randomList)
          (replicate generations nextGen)

batchGeneticMinPathDistance :: Int -> Int -> [[Point]] -> [Int]
batchGeneticMinPathDistance p g =
  map
    (geneticMinPathDistance p g)

batchParallelGeneticMinPathDistance :: Int -> Int -> [[Point]] -> [Int]
batchParallelGeneticMinPathDistance p g =
  parMap
    rseq
    (geneticMinPathDistance p g)

runMain :: IO ()
runMain = do
  args <- getArgs
  case args of
    -- bruteforce sequential
    ["-s", filename] -> do
      corpus <- readFile filename
      print $ minPathDistance $ makeCities corpus

    -- bruteforce, calculate path distance in parallel
    ["-p", filename] -> do
      corpus <- readFile filename
      print $ parallelMinPathDistance $ makeCities corpus

    -- bruteforce, calculate path distance in parallel chunks
    ["-c", ':' : 'n' : n, filename] -> do
      corpus <- readFile filename
      print $ chunkedParallelMinPathDistance (makeCities corpus) (read n)

    -- bruteforce for batch of city groups
    ["-s", ':' : 'b' : b, filename] -> do
      corpus <- readFile filename
      let cities = makeCities corpus
          randomList = randomListInRange 0 (length cities)
       in print $ batchMinPathDistance [take r cities | r <- take (read b) randomList]

    -- bruteforce for batch of city groups, each group in parallel
    ["-sp", ':' : 'b' : b, filename] -> do
      corpus <- readFile filename
      let cities = makeCities corpus
          randomList = randomListInRange 0 (length cities)
       in print $ batchParallelMinPathDistance [take r cities | r <- take (read b) randomList]

    -- genetic algorithm
    ["-g", ':' : 'p' : p, ':' : 'g' : g, filename] -> do
      corpus <- readFile filename
      print $ geneticMinPathDistance (read p) (read g) $ makeCities corpus

    -- genetic algorithm for batch of city gorups
    ["-g", ':' : 'p' : p, ':' : 'g' : g, ':' : 'b' : b, filename] -> do
      corpus <- readFile filename
      let cities = makeCities corpus
          randomList = randomListInRange 0 (length cities)
       in print $ batchGeneticMinPathDistance (read p) (read g) [take r cities | r <- take (read b) randomList]

    -- genetic algorithm for batch of city gorups, each group in parallel
    ["-gp", ':' : 'p' : p, ':' : 'g' : g, ':' : 'b' : b, filename] -> do
      corpus <- readFile filename
      let cities = makeCities corpus
          randomList = randomListInRange 0 (length cities)
       in print $ batchParallelGeneticMinPathDistance (read p) (read g) [take r cities | r <- take (read b) randomList]

    -- invalid running params
    _ -> do
      pn <- getProgName
      die $ "Usage: " ++ pn ++ " [-s|-p|-c :nN|-s :bN|-sp :bN|-g :pN :gN|-g :pN :gN :bN|-gp :pN :gN :bN] <filename>"
