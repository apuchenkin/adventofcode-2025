module Main where

import qualified Day1
-- import qualified Day10
-- import qualified Day11
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
-- import qualified Day6
-- import qualified Day7
-- import qualified Day8
-- import qualified Day9
import System.Environment (getArgs)

type Solution a = String -> a

data Day a = Day
  { first :: Solution a,
    second :: Solution a
  }

getDay :: String -> Day (IO String)
getDay idx = case idx of
  "1" -> Day (return . Day1.first) (return . Day1.second)
  "2" -> Day (return . Day2.first) (return . Day2.second)
  "3" -> Day (return . Day3.first) (return . Day3.second)
  "4" -> Day (return . Day4.first) (return . Day4.second)
  "5" -> Day (return . Day5.first) (return . Day5.second)
--   "6" -> Day (return . Day6.first) (return . Day6.second)
--   "7" -> Day (return . Day7.first) (return . Day7.second)
--   "8" -> Day (return . Day8.first) (return . Day8.second)
--   "9" -> Day (return . Day9.first) (return . Day9.second)
--   "10" -> Day (return . Day10.first) (return . Day10.second)
--   "11" -> Day (return . Day11.first) (return . Day11.second)
--   "12" -> Day (return . Day12.first) (return . Day12.second)
  _ -> error "day unsupported"

main :: IO ()
main = do
  args <- getArgs
  let idx = head args
  let day = getDay idx
  file <- readFile $ "input/day" ++ idx ++ ".txt"
  first' <- first day file
  putStrLn first'
  second' <- second day file
  putStrLn second'