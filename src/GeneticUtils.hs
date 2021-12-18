module GeneticUtils (nextGen) where

import Data.List (sortBy)
import qualified Data.Set as S
import Types
import Utils

crossover :: [Point] -> [Point] -> [Point]
crossover parentA parentB = c1 ++ c2
  where
    s = getRandomFromRange 0 (length parentA - 1)
    e = getRandomFromRange (s + 1) (length parentA - 1)
    c1 = [x | (x, i) <- zip parentA [0 ..], s <= i && i <= e]
    c1Set = S.fromList c1
    c2 = [x | x <- parentB, not (x `S.member` c1Set)]

nextGen :: [[Point]] -> [[Point]]
nextGen pop =
  take (length pop) $
    map fst $
      sortBy (\p1 p2 -> compare (snd p1) (snd p2)) $
        map (\p -> (p, squaredPathDistance p)) $
          [crossover pa pb | (i, pa) <- zip [0 ..] pop, (j, pb) <- zip [0 ..] pop, i < j]
