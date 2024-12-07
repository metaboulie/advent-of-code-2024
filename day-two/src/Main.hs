module Main (main) where

import qualified Data.List as L

type Row = [Int]
type Matrix = [Row]

parseMatrix :: String -> Matrix
parseMatrix = map (map read . words) . lines

diffRow :: Row -> Row
diffRow xs = zipWith (-) (tail xs) xs

diffMatrix :: Matrix -> Matrix
diffMatrix = map diffRow

inRange :: Int -> Int -> Int -> Bool
inRange a b x = x >= a && x <= b

checkRow :: Row -> Int
checkRow xs
    | all (inRange 1 3) xs = 1
    | all (inRange (-3) (-1)) xs = 1
    | otherwise = 0

pairSums :: Row -> [Row]
pairSums [] = []
pairSums [_] = []
pairSums (x : y : rest) =
    (sum [x, y] : rest) : map (x :) (pairSums (y : rest))

checkRowTransforms :: Row -> Int
checkRowTransforms row
    | null row = 0
    | checkRow row == 1 = 1
    | checkRow (tail row) == 1 = 1 -- remove first
    | checkRow (init row) == 1 = 1 -- remove last
    | any (\r -> checkRow r == 1) (pairSums row) = 1 -- check all possible pair sums
    | otherwise = 0

checkMatrix :: Matrix -> [Int]
checkMatrix = map checkRow

checkMatrixTransforms :: Matrix -> [Int]
checkMatrixTransforms = map checkRowTransforms

main :: IO ()
main = do
    matrix <- parseMatrix <$> readFile "inputs.txt"
    let questionOneResult = L.sum $ checkMatrix $ diffMatrix matrix
    let questionTwoResult = L.sum $ checkMatrixTransforms $ diffMatrix matrix

    putStrLn $ "answer to question 1: " ++ show questionOneResult -- correct answer: 252
    putStrLn $ "answer to question 2: " ++ show questionTwoResult -- correct answer: 324
