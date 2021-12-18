module GeneticUtils (shuffle, fitness, nextGen) where

import Data.List (sortBy)
import GHC.IO (unsafePerformIO)
import System.Random (getStdRandom, randomR)
import Types
import Utils

shuffle :: [a] -> [a]
shuffle x
  | length x < 2 = x
  | otherwise = (x !! i) : shuffle (take i x ++ drop (i + 1) x)
  where
    i = getRandomFromRange 0 (length x - 1)

fitness :: [Point] -> Int
fitness cities = 10 ^ 5 `div` d ^ 3 + 1
  where
    d = pathDistance cities

crossover :: [Point] -> [Point] -> [Point]
crossover parentA parentB = c1 ++ c2
  where
    s = getRandomFromRange 0 (length parentA - 1)
    e = getRandomFromRange (s + 1) (length parentA - 1)
    c1 = [x | (x, i) <- zip parentA [0 ..], s <= i && i <= e]
    c2 = [x | x <- parentB, x `notElem` c1]

nextGen :: [[Point]] -> [[Point]]
nextGen pop =
  take (length pop) $
    sortBy
      (\p1 p2 -> compare (pathDistance p1) (pathDistance p2))
      [crossover pa pb | (i, pa) <- zip [0 ..] pop, (j, pb) <- zip [0 ..] pop, i < j]
