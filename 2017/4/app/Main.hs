module Main where

import           Lib

main :: IO ()
main = do
  puzzleContents <- readFile "puzzle.txt"
  let puzzle = filter (not . null) (lines puzzleContents)
  print (part1 puzzle)
  print (part2 puzzle)
