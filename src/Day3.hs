module Day3 (first, second) where

parse :: String -> [[Int]]
parse text = fmap (read . (: [])) <$> res
  where
    res = lines text

first :: String -> String
first input = show $ sum $ read . concatMap show . maximumN 2 <$> parse input

maximumN :: (Ord a) => Int -> [a] -> [a]
maximumN 1 word = [maximum word]
maximumN n word = first : maximumN (n - 1) (tail rest)
  where
    first = maximum $ iterate init word !! (n - 1)
    (_, rest) = break (== first) word

second :: String -> String
second input = show $ sum $ read . concatMap show . maximumN 12 <$> parse input