module Main (main) where

import qualified Data.List as L
import Text.Regex.TDFA

data SearchState = SearchMulAndDont | SearchDo

puzzle :: String -> Int
puzzle s = L.foldl' (+) 0 . map (\(x, y) -> x * y) $ mull_pairs
  where
    regex = "mul\\(([[:digit:]]{1,3}),([[:digit:]]{1,3})\\)"

    findMulWithRemainder :: String -> Maybe ((Int, Int), String)
    findMulWithRemainder s' =
        let (_, _, after, digits) = (s' =~ regex) :: (String, String, String, [String])
         in if null digits
                then Nothing
                else Just ((read . head $ digits, read . head . tail $ digits), after)

    mull_pairs = L.unfoldr findMulWithRemainder s

puzzleTwo :: String -> Int
puzzleTwo s = go SearchMulAndDont 0 s
  where
    mul_pattern = "mul\\(([[:digit:]]{1,3}),([[:digit:]]{1,3})\\)"
    dont_pattern = "don't\\(\\)"
    do_pattern = "do\\(\\)"

    processMuls :: String -> Int -> (Int, String)
    processMuls s' acc =
        let (pre_mul, match_mul, after_mul, groups_mul) =
                s' =~ mul_pattern :: (String, String, String, [String])
         in case groups_mul of
                (x : y : _) -> processMuls after_mul (acc + read x * read y)
                _ -> (acc, s')

    go :: SearchState -> Int -> String -> Int
    go state acc s'
        | null s' = acc
        | otherwise = case state of
            SearchMulAndDont ->
                let (pre_dont, match_dont, after_dont, groups_dont) =
                        s' =~ dont_pattern :: (String, String, String, [String])
                 in if not (null match_dont)
                        then
                            let (new_acc, _) = processMuls pre_dont acc
                             in go SearchDo new_acc after_dont
                        else
                            let (new_acc, _) = processMuls s' acc
                             in new_acc
            SearchDo ->
                let (pre_do, match_do, after_do, groups_do) =
                        s' =~ do_pattern :: (String, String, String, [String])
                 in if not (null match_do)
                        then go SearchMulAndDont acc after_do
                        else acc

main :: IO ()
main = do
    contents <- readFile "inputs.txt"

    let questionOneResult = puzzle contents
        questionTwoResult = puzzleTwo contents

    putStrLn $ "answer to question 1: " ++ show questionOneResult -- correct answer: 168539636
    putStrLn $ "answer to question 2: " ++ show questionTwoResult -- correct answer: 97529391
