module Main (main) where

import Text.Regex.TDFA

searchPattern :: String -> Int
searchPattern input = go input 0
  where
    go :: String -> Int -> Int
    go str acc = case str =~ "X" :: (String, String, String) of
        (_, "", _) -> acc -- No X found
        (_, _, afterX) -> searchM afterX acc

    searchX :: String -> Int -> Int
    searchX str acc = case str =~ "X" :: (String, String, String) of
        (_, "", _) -> acc -- No X found
        (_, _, afterM) -> searchA afterM acc

    searchM :: String -> Int -> Int
    searchM str acc = case str =~ "M" :: (String, String, String) of
        (_, "", _) -> acc -- No M found
        (_, _, afterM) -> searchA afterM acc

    searchA :: String -> Int -> Int
    searchA str acc = case str =~ "A" :: (String, String, String) of
        (_, "", _) -> acc -- No A found
        (_, _, afterA) -> searchS afterA acc

    searchS :: String -> Int -> Int
    searchS str acc = case str =~ "S" :: (String, String, String) of
        (_, "", _) -> acc -- No S found
        (_, _, afterS) -> go afterS (acc + 1) -- Increment counter and continue search

main :: IO ()
main = do
    let test = "XMASXMASXMAS"
    print $
        searchPattern test -- Will print 3
