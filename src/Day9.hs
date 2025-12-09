module Day9 (first, second) where

import qualified Control.Monad.State as ST
import Data.Function (on)
import Data.List (maximumBy, nub, sort)
import Data.List.Split (splitOn)

type Coordinate = (Int, Int)

type Path = (Coordinate, Coordinate)

type Box = (Coordinate, Coordinate)

type Instruction = (Turn, Path)

data Turn = L | R
  deriving (Eq, Show, Ord)

data Direction = N | S | E | W
  deriving (Eq, Show, Ord)

parse :: String -> [Coordinate]
parse = map parseCoord . lines
  where
    parseCoord s = case splitOn "," s of
      [x, y] -> (read x, read y)
      _ -> error $ "Invalid coordinate: " ++ s

area :: Box -> Int
area ((x1, y1), (x2, y2)) = succ (abs (x1 - x2)) * succ (abs (y1 - y2))

first :: String -> String
first input = show $ area $ maximumBy (compare `on` area) pairs
  where
    coordinates = parse input
    pairs = [(a, b) | a <- coordinates, b <- coordinates, a < b]

paths :: [Coordinate] -> [(Turn, Path)]
paths coordinates = zipWith (\prev next -> (readTurn prev next, next)) (last pairs : init pairs) pairs
  where
    pairs = zip coordinates (tail coordinates ++ [head coordinates])
    readTurn :: Path -> Path -> Turn
    readTurn prev next = turnMap (direction prev) (direction next)
    turnMap :: Direction -> Direction -> Turn
    turnMap N W = L
    turnMap N E = R
    turnMap S W = R
    turnMap S E = L
    turnMap W S = L
    turnMap W N = R
    turnMap E S = R
    turnMap E N = L
    turnMap d1 d2 = error $ "invalid direction: " ++ show (d1, d2)

direction :: Path -> Direction
direction ((x1, y1), (x2, y2))
  | x1 == x2 = if y2 < y1 then N else S
  | x1 < x2 = E
  | otherwise = W

toBox :: [Path] -> Box
toBox paths' = ((minimum xs, minimum ys), (maximum xs, maximum ys))
  where
    coordinates = concatMap (\(left, right) -> [left, right]) paths'
    (xs, ys) = unzip coordinates

pathLength :: Path -> Int
pathLength ((x1, y1), (x2, y2)) = abs (x1 - x2) + abs (y1 - y2)

splitPath :: Path -> Path -> (Path, Path)
splitPath ((x1, y1), (x2, y2)) ((x3, y3), (x4, y4))
  | x3 == x4 = (((x1, y1), (x3, y1)), ((x3, y2), (x2, y2)))
  | y3 == y4 = (((x1, y1), (x1, y3)), ((x2, y3), (x2, y2)))
  | otherwise = error "Invalid path"

simplify :: [Instruction] -> ST.State [Box] [Instruction]
simplify [] = return []
simplify instructions
  | length instructions <= 3 = return instructions
simplify instructions@((d, steps) : (R, steps0) : (R, steps1) : (L, steps2) : zs)
  | pathLength steps > pathLength steps1 = do
      let (left, _) = splitPath steps steps2
      ST.modify (toBox [steps0, steps1] :)
      simplify $ (d, left) : (R, (snd left, snd steps2)) : zs
  | otherwise = (head instructions :) <$> simplify (tail instructions)
simplify instructions@((d, steps) : (L, steps0) : (R, steps1) : (R, steps2) : zs)
  | pathLength steps2 > pathLength steps0 = do
      let (left, _) = splitPath steps2 steps
      ST.modify (toBox [steps0, steps1] :)
      simplify $ (d, (fst steps, snd left)) : (R, (snd left, snd steps2)) : zs
  | otherwise = (head instructions :) <$> simplify (tail instructions)
simplify (x : zs) = (x :) <$> simplify zs

consistsOf :: Box -> [Box] -> Bool
consistsOf box@(left, right) boxes = case gluedBoxes of
  [b] -> b == box
  _ -> False
  where
    boxes' = nub $ concatMap (splitXY right) $ concatMap (splitXY left) boxes
    insideBoxes = sort $ filter (inside box) boxes'
    gluedBoxes = glueBoxes insideBoxes

glueX :: Box -> Box -> [Box]
glueX box1@((xl, yl), (xl', yl')) box2@((xr, yr), (xr', yr'))
  | yl == yr && yl' == yr' && xl' == xr = [((xl, yl), (xr', yr'))]
  | otherwise = [box1, box2]

glueY :: Box -> Box -> [Box]
glueY box1@((xl, yl), (xl', yl')) box2@((xr, yr), (xr', yr'))
  | xl == xr && xl' == xr' && yl' == yr = [((xl, yl), (xr', yr'))]
  | otherwise = [box1, box2]

glueXY :: Box -> Box -> [Box]
glueXY l r
  | [box] <- glueX l r = [box]
  | [box] <- glueY l r = [box]
  | otherwise = [l, r]

glueBoxes :: [Box] -> [Box]
glueBoxes [] = []
glueBoxes [x] = [x]
glueBoxes (x : y : zs) = case glueXY x y of
  [z] -> glueBoxes $ z : zs
  _ -> x : y : glueBoxes zs

inside :: Box -> Box -> Bool
inside ((xl, yl), (xl', yl')) ((xr, yr), (xr', yr')) =
  xr >= xl && xr' <= xl' && yr >= yl && yr' <= yl'

splitX :: Int -> Box -> [Box]
splitX x box@((x1, y1), (x2, y2))
  | x <= x1 || x >= x2 = [box]
  | otherwise = [left, right]
  where
    left = ((x1, y1), (x, y2))
    right = ((x, y1), (x2, y2))

splitY :: Int -> Box -> [Box]
splitY y box@((x1, y1), (x2, y2))
  | y <= y1 || y >= y2 = [box]
  | otherwise = [left, right]
  where
    left = ((x1, y1), (x2, y))
    right = ((x1, y), (x2, y2))

splitXY :: Coordinate -> Box -> [Box]
splitXY (x, y) box = nub $ concatMap (splitY y) (splitX x box)

second :: String -> String
second input = show $ maximum $ area <$> pairs
  where
    coordinates = parse input
    (instructions', boxes) = simplifyUntilDone (paths coordinates) []
    boxes' = toBox (snd <$> instructions') : boxes
    pairs = [(a, b) | a <- coordinates, b <- coordinates, a < b, consistsOf (toBox [(a, b)]) boxes']
    simplifyUntilDone insts bs
      | length insts <= 4 = (insts, bs)
      | otherwise = simplifyUntilDone newInsts newBoxes
      where
        (newInsts, newBoxes) = ST.runState (simplify $ tail insts ++ [head insts]) bs