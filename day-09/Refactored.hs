module Main where

import qualified Data.Char as C
import qualified Data.Vector.Unboxed as V

data ParseState = ParseBlock | ParseFreeSpace

parseIntoBlocks :: V.Vector Int -> V.Vector Int
parseIntoBlocks input = V.create $ do
    let len = V.length input
    vec <- V.thaw $ V.replicate len (-1)
    go vec 0 0 ParseBlock
    return vec
  where
    go vec pos fileId state
        | pos >= V.length input = vec
        | otherwise = case state of
            ParseBlock -> do
                let num = input V.! pos
                V.slice pos num vec `V.map` fileId
                go vec (pos + num) (fileId + 1) ParseFreeSpace
            ParseFreeSpace ->
                go vec (pos + (input V.! pos)) fileId ParseBlock

tidySystem :: V.Vector Int -> V.Vector Int
tidySystem = V.filter (/= -1)

getCheckSum :: V.Vector Int -> Int
getCheckSum = V.ifoldr (\i x acc -> acc + i * x) 0

main :: IO ()
main = do
    content <- readFile "inputs.txt"
    let diskMap = V.fromList $ map C.digitToInt $ filter C.isDigit content
        result = getCheckSum $ tidySystem $ parseIntoBlocks diskMap
    print result
