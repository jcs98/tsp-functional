module GeneticUtils (nextGen) where

import Data.List (sortBy)
import qualified Data.Set as S
import Types
import Utils

crossover :: [Point] -> [Point] -> Int -> Int -> [Point]
crossover parentA parentB i j = c1 ++ c2
  where
    s = min i j
    e = max i j
    c1 = [x | (x, i) <- zip parentA [0 ..], s <= i && i <= e]
    c1Set = S.fromList c1
    c2 = [x | x <- parentB, not (x `S.member` c1Set)]

nextGen :: [[Point]] -> [Int] -> [[Point]]
nextGen pop randomList =
  take (length pop) $
    map fst $
      sortBy (\p1 p2 -> compare (snd p1) (snd p2)) $
        map (\p -> (p, squaredPathDistance p)) $
          [ crossover pa pb ri rj
            | (i, pa, ri) <- zip3 [0 ..] pop randomList,
              (j, pb, rj) <- zip3 [0 ..] pop (tail randomList),
              i < j
          ]
