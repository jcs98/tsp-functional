module Lib
    ( runMain
    ) where

import System.IO(readFile)
import System.Environment(getArgs, getProgName)
import System.Exit(die)
import Data.List(permutations)

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

minPathDistance :: [Point] -> Int
minPathDistance cities = minimum $ map pathDistance $ permutations cities

runMain :: IO ()
runMain = do
   args <- getArgs
   case args of
      [filename] -> do
         corpus <- readFile filename
         putStrLn $ show $ minPathDistance $ makeCities corpus
      _ -> do
         pn <- getProgName
         die $ "Usage: " ++ pn ++ " <filename>"
