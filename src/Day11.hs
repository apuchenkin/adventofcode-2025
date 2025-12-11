{-# LANGUAGE RecordWildCards #-}

module Day11 (first, second) where

import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
import Topograph (G (G, gFromVertex, gToVertex), allPaths, runG')

type Label = String

parse :: String -> M.Map Label (S.Set Label)
parse text = adjMap `M.union` M.fromSet (const S.empty) allNodes
  where
    adjMap = M.fromList $ readLine <$> lines text
    allNodes = foldl1 S.union (M.elems adjMap)
    readLine line = case splitOn ": " line of
      [label, deps] -> (label, S.fromList $ splitOn " " deps)
      _ -> error $ "Invalid line: " ++ line

paths :: M.Map Label (S.Set Label) -> Label -> Label -> Int
paths adjMap from to = fromMaybe 0 (fmap length =<< result)
  where
    result = runG' adjMap $ \g@G {..} ->
      (fmap . fmap . fmap) gFromVertex $ allPaths g <$> gToVertex from <*> gToVertex to

first :: String -> String
first input = show $ paths (parse input) "you" "out"

second :: String -> String
second input = show $ product $ uncurry (paths adjMap) <$> pairs
  where
    adjMap = parse input
    pairs = [("svr", "fft"), ("fft", "dac"), ("dac", "out")]
