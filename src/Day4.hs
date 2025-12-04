module Day4 (first, second) where

import qualified Data.Matrix as M

type Coordinate = (Int, Int)

type Plot = M.Matrix Char

adjacent :: Coordinate -> [Coordinate]
adjacent (row, col) = [(r, c) | r <- [row - 1 .. row + 1], c <- [col - 1 .. col + 1], (r, c) /= (row, col)]

parse :: String -> Plot
parse = M.fromLists . lines

isWall :: Plot -> Coordinate -> Bool
isWall plot c = count < 4
  where
    count = length $ filter id $ isWall' plot <$> adjacent c
    isWall' p (y, x) = M.safeGet y x p == Just '@'

first :: String -> String
first = show . countElems 'x' . step . parse

step :: Plot -> Plot
step plot = M.mapPos (\c v -> if v == '@' && isWall plot c then 'x' else v) plot

clear :: Plot -> Plot
clear = M.mapPos (\_ v -> if v == 'x' then '.' else v)

countElems :: (Eq a) => a -> M.Matrix a -> Int
countElems x plot = length $ filter (== x) $ M.toList plot

second :: String -> String
second input = show $ initialCount - finalCount
  where
    plot = parse input
    initialCount = countElems '@' plot
    finalPlot = until (notElem 'x' . M.toList . step) (clear . step) plot
    finalCount = countElems '@' finalPlot