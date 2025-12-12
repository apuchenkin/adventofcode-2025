module Day12 (first, second) where

import Data.List.Split (splitOn)

type Dimensions = (Int, Int)

type Requirements = [Int]

type Item = (Dimensions, Requirements)

parse :: String -> [Item]
parse text = readItem <$> res
  where
    items = splitOn "\n\n" text
    res = lines $ last items
    readItem line = ((x, y), requirements)
      where
        [left, right] = splitOn ": " line
        requirements = read <$> splitOn " " right
        [x, y] = read <$> splitOn "x" left

isValid :: Item -> Bool
isValid ((x, y), requirements) = x * y >= sum requirements * 9

first :: String -> String
first input = show $ length $ filter isValid n
  where
    n = parse input

second :: String -> String
second _ = show '*'