{-# LANGUAGE TupleSections #-}

module Main where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as L

parseInputPairs :: String -> ([Int], [Int])
parseInputPairs = unzip . map ((\[x, y] -> (x, y)) . map read . words) . lines

calculateAbsoluteDifferences :: [Int] -> [Int] -> [Int]
calculateAbsoluteDifferences = zipWith ((abs .) . (-))

createFrequencyMap :: [Int] -> HashMap.HashMap Int Int
createFrequencyMap = HashMap.fromListWith (+) . map (,1)

calculateWeightedIntersection :: [Int] -> [Int] -> Int
calculateWeightedIntersection xs ys = sum $ HashMap.intersectionWith (*) weightedFreqMap targetFreqMap
  where
    weightedFreqMap = HashMap.mapWithKey (*) sourceFreqMap
    targetFreqMap = createFrequencyMap ys
    sourceFreqMap = createFrequencyMap xs

main :: IO ()
main = do
    (sourceNumbers, targetNumbers) <- parseInputPairs <$> readFile "inputs.txt"

    let (sortedSourceNumbers, sortedTargetNumbers) = (L.sort sourceNumbers, L.sort targetNumbers)
        totalDifference = L.sum $ calculateAbsoluteDifferences sortedSourceNumbers sortedTargetNumbers
        weightedIntersectionSum = calculateWeightedIntersection sourceNumbers targetNumbers

    putStrLn $ "answer to question 1: " ++ show totalDifference
    putStrLn $ "answer to question 2: " ++ show weightedIntersectionSum
