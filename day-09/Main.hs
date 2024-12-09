-- part 1
module Main where

import qualified Data.Char as C
import qualified Data.List as L

data ParseState = ParseBlock | ParseFreeSpace deriving (Eq)

parseIntoBlocks :: [Int] -> [Int]
parseIntoBlocks diskMap = parseIntoBlocks' diskMap 0 ParseBlock
  where
    parseIntoBlocks' :: [Int] -> Int -> ParseState -> [Int]
    parseIntoBlocks' [] _ _ = []
    parseIntoBlocks' (num : rest) fileID ParseBlock = replicate num fileID ++ parseIntoBlocks' rest (fileID + 1) ParseFreeSpace
    parseIntoBlocks' (num : rest) fileID ParseFreeSpace = replicate num (-1) ++ parseIntoBlocks' rest fileID ParseBlock

tidySystemOptimized :: [Int] -> [Int]
tidySystemOptimized = go []
  where
    go acc [] = reverse acc
    go acc [x] = reverse (x : acc)
    go acc (x : xs)
        | x == -1 = case dropWhile (== -1) xs of
            [] -> reverse acc
            ys -> go acc (init ys ++ [last ys])
        | otherwise = go (x : acc) xs

tidySystem :: [Int] -> [Int]
tidySystem system = tidySystem' system []
  where
    removeFreeSpaceFromTail :: [Int] -> [Int]
    removeFreeSpaceFromTail = reverse . dropWhile (== -1) . reverse

    tidySystem' :: [Int] -> [Int] -> [Int]
    tidySystem' [] acc = reverse acc
    tidySystem' [x] acc = reverse $ x : acc
    tidySystem' (x : xs) acc
        | x == -1 = case removeFreeSpaceFromTail xs of
            [] -> tidySystem' [] acc
            trimmed -> tidySystem' (init trimmed) (last trimmed : acc)
        | otherwise = tidySystem' xs (x : acc)

getCheckSum :: [Int] -> Int
getCheckSum xs = sum $ zipWith (*) xs [0 ..]

main :: IO ()
main = do
    content <- readFile "test.txt"
    let diskMap = map C.digitToInt $ filter C.isDigit content
        system = parseIntoBlocks diskMap
        tidyedSystem = tidySystem system
        checkedSum = getCheckSum tidyedSystem
    putStrLn $ "checked sum: " ++ show checkedSum
