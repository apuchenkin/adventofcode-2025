module Day7 (first, second) where

import qualified Data.Matrix as MX
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Map as M
import qualified Control.Monad.State as S

type Plot = MX.Matrix Char
type Beam = (Int, Int)

parse :: String -> Plot
parse = MX.fromLists . lines

find :: Char -> Plot -> (Int, Int)
find c plot = case listToMaybe $ catMaybes $ MX.toList $ MX.mapPos (\(y, x) v -> if v == c then Just (y, x) else Nothing) plot of
  Just pos -> pos
  Nothing -> error $ "Character '" ++ [c] ++ "' not found in plot"

step :: Beam -> Plot -> Plot
step (y, x) plot = case MX.safeGet y x plot of
  Just 'S' -> step (y + 1, x) plot
  Just '|' -> plot
  Just '^' -> step (y, x - 1) $ step (y, x + 1) plot
  Just '.' -> step (y + 1, x) $ MX.setElem '|' (y, x) plot
  _ -> plot

type Cache = M.Map (Int, Int) Int

step1 :: Beam -> Plot -> S.State Cache (Plot, Int)
step1 (y, x) plot = case MX.safeGet y x plot of
  Just 'S' -> step1 (y + 1, x) plot
  Just '|' -> step1 (y + 1, x) plot
  Just '^' -> do
    mem <- S.gets $ M.lookup (y, x)
    case mem of
      Just v -> return (plot, v)
      Nothing -> do
        (plot', n') <- step1 (y, x - 1) plot
        (plot'', n'') <- step1 (y, x + 1) plot'
        let total = n' + n''
        S.modify $ M.insert (y, x) total
        return (plot'', total)
  Just '.' -> step1 (y + 1, x) $ MX.setElem '|' (y, x) plot
  _ -> return (plot, 1)

isSplit :: Plot -> (Int, Int) -> Bool
isSplit plot (y, x) = MX.safeGet (y - 1) x plot == Just '|'

findSplits :: Plot -> [(Int, Int)]
findSplits plot = catMaybes $ MX.toList $ MX.mapPos (\(y, x) v -> if v == '^' && isSplit plot (y, x) then Just (y, x) else Nothing) plot

first :: String -> String
first input = show $ length $ findSplits plot'
  where
    plot = parse input
    plot' = step (find 'S' plot) plot

second :: String -> String
second input = show $ snd $ S.evalState (step1 (find 'S' plot) plot) M.empty
  where
    plot = parse input 