module Main where

import           Data.List.Split
import           Lib

main :: IO ()
main = do
  puzzleContents <- readFile "puzzle.txt"
  let puzzle = filter (not.null) (map (map (read::String->Int) . words) (splitOn "\n" puzzleContents))
  print (part1 puzzle)
  print (part2 puzzle)
