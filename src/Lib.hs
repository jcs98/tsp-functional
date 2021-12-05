module Lib
    ( runMain
    ) where

type Point = (Int, Int)

squaredDistance :: Point -> Point -> Int
squaredDistance (x1, y1) (x2, y2) = ((x2 - x1) ^ 2) + ((y2 - y1) ^ 2)

distance :: Point -> Point -> Int
distance a b = floor . sqrt . fromIntegral $ squaredDistance a b

makeCities :: String -> [Point]
makeCities corpus = makePairs $ map read $ words corpus
    where
        makePairs [] = []
        makePairs (p:q:r) = (p, q):(makePairs r)

pathDistance :: [Point] -> Int
pathDistance cities = sum $ zipWith distance path (tail path)
    where path = (last cities):cities 

runMain :: IO ()
runMain = putStrLn $ show $ pathDistance $ makeCities $ "0 0 3 4 6 8"
