module Day5 (first, second) where

import Data.List (sortOn)
import Data.List.Split (splitOn)

type Range = (Int, Int)

parse :: String -> ([Range], [Int])
parse text = (parseRange <$> lines ranges, read <$> lines ingridients)
  where
    [ranges, ingridients] = splitOn "\n\n" text
    parseRange line = let [a, b] = read <$> splitOn "-" line in (a, b)

isInRange :: Int -> Range -> Bool
isInRange n (a, b) = n >= a && n <= b

union :: Range -> Range -> Range
union (a1, b1) (a2, b2) = (min a1 a2, max b1 b2)

isOverlap :: Range -> Range -> Bool
isOverlap (a1, b1) (a2, b2) = a1 <= b2 && a2 <= b1

first :: String -> String
first input = show $ length $ filter (flip any ranges . isInRange) ingridients
  where
    (ranges, ingridients) = parse input

size :: Range -> Int
size (a, b) = b - a + 1

mergeRanges :: [Range] -> [Range]
mergeRanges [] = []
mergeRanges [r] = [r]
mergeRanges (r1 : r2 : rest)
  | isOverlap r1 r2 = mergeRanges (union r1 r2 : rest)
  | otherwise = r1 : mergeRanges (r2 : rest)

second :: String -> String
second input = show $ sum $ size <$> mergeRanges (sortOn fst ranges)
  where
    (ranges, _) = parse input