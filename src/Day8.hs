module Day8 (first, second) where

import Data.Function (on)
import qualified Data.Graph as G
import Data.List (minimumBy, sortOn)
import Data.List.Split (splitOn)
import Data.Ord (Down (..))
import qualified Data.Set as S

type Coordinate = (Int, Int, Int)
type Path = (Coordinate, Coordinate)

parse :: String -> [Coordinate]
parse = map ((\[a, b, c] -> (a, b, c)) . map read . splitOn ",") . lines

-- Use squared distance to avoid sqrt (same ordering, more efficient)
distanceSq :: Coordinate -> Coordinate -> Int
distanceSq (x1, y1, z1) (x2, y2, z2) = dx * dx + dy * dy + dz * dz
  where
    dx = x1 - x2
    dy = y1 - y2
    dz = z1 - z2

shortestPath :: S.Set Path -> Path
shortestPath = minimumBy (compare `on` uncurry distanceSq)

connect' :: ([Path], S.Set Path) -> ([Path], S.Set Path)
connect' (connected, paths)
  | null paths = (connected, S.empty)
  | otherwise = (path : connected, S.delete path paths)
  where
    path = shortestPath paths

connectedVertices :: Coordinate -> [Path] -> S.Set Coordinate
connectedVertices c paths = S.fromList [v | (a, b) <- paths, a == c || b == c, v <- [a, b], v /= c]

toVertices :: [Path] -> S.Set Coordinate
toVertices paths = S.fromList [v | (a, b) <- paths, v <- [a, b]]

toGraph :: [Path] -> G.Graph
toGraph paths = graph
  where
    vertices = S.toList $ toVertices paths
    (graph, _, _) = G.graphFromEdges [(v, v, S.toList $ connectedVertices v paths) | v <- vertices]

allPairs :: [Coordinate] -> S.Set Path
allPairs coordinates = S.fromList [(a, b) | a <- coordinates, b <- coordinates, a < b]

first :: String -> String
first input = show $ product $ take 3 $ sortOn Down $ map length $ G.components $ toGraph paths
  where
    coordinates = parse input
    (paths, _) = iterate connect' ([], allPairs coordinates) !! 1000

second :: String -> String
second input = show $ (\((x, _, _), (y, _, _)) -> x * y) $ head paths
  where
    coordinates = parse input
    (paths, _) = until
      (\(a, _) -> toVertices a == S.fromList coordinates && length (G.components $ toGraph a) == 1)
      connect'
      ([], allPairs coordinates)