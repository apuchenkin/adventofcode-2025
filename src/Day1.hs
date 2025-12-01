module Day1 (first, second) where

parse :: String -> [Int]
parse = map parseLine . lines
  where
    parseLine ('L' : xs) = -(read xs)
    parseLine ('R' : xs) = read xs
    parseLine _ = 0

countZeros :: [Int] -> Int
countZeros = length . filter (== 0) . scanl (\a b -> (a + b) `mod` 100) 50

first :: String -> String
first = show . countZeros . parse

second :: String -> String
second = show . countZeros . concatMap expand . parse
  where
    expand x = replicate (abs x) (signum x)