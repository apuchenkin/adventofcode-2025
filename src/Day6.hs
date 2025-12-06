module Day6 (first, second) where

import Data.List (transpose)
import Data.List.Split (splitWhen)

parse :: String -> ([[Int]], [String])
parse text = (transpose $ fmap read <$> numbers, ops)
  where
    dats = words <$> lines text
    numbers = init dats
    ops = last dats

parse2 :: String -> ([[Int]], [String])
parse2 text = (concat <$> splitWhen null numbers, ops)
  where
    numbers = fmap read . words <$> transpose (init $ lines text)
    ops = words . last . lines $ text

applyOp :: String -> Int -> Int -> Int
applyOp "+" = (+)
applyOp "*" = (*)
applyOp _ = error "Invalid operation"

solve :: (String -> ([[Int]], [String])) -> String -> String
solve parseFunc input = show . sum $ zipWith (foldl1 . applyOp) ops numbers
  where
    (numbers, ops) = parseFunc input

first :: String -> String
first = solve parse

second :: String -> String
second = solve parse2