module Utils
  ( distance,
    squaredDistance,
    makeCities,
    pathDistance,
    squaredPathDistance,
    getRandomFromRange,
  )
where

import GHC.IO (unsafePerformIO)
import System.Random (getStdRandom, randomR)
import Types

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

squaredPathDistance :: [Point] -> Int
squaredPathDistance cities = sum $ zipWith squaredDistance path (tail path)
  where
    path = last cities : cities

getRandomFromRange :: Int -> Int -> Int
getRandomFromRange s e = unsafePerformIO $ getStdRandom $ randomR (s, e)
