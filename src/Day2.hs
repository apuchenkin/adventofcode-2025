module Day2 (first, second) where

import Data.List.Split (splitOn)

parse :: String -> [(Int, Int)]
parse = map readRange . splitOn ","
  where
    readRange line = case splitOn "-" line of
      [left, right] -> (read left, read right)
      _ -> error "Invalid range format"

isValid :: Int -> Bool
isValid n = left /= right
  where
    number = show n
    (left, right) = splitAt (length number `div` 2) number

isValid2 :: Int -> Bool
isValid2 n = not $ any isRepeating seqs
  where
    number = show n
    seqs = take <$> [1 .. length number `div` 2] <*> [number]
    isRepeating s = number == concat (replicate (length number `div` length s) s)

expand :: (Int, Int) -> [Int]
expand (a, b) = [a .. b]

first :: String -> String
first = show . sum . concatMap (filter (not . isValid) . expand) . parse

second :: String -> String
second = show . sum . concatMap (filter (not . isValid2) . expand) . parse