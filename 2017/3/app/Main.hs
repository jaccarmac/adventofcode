module Main where

import           Lib

main :: IO ()
main = do
  puzzleContents <- readFile "puzzle.txt"
  let puzzle = puzzleContents
  print (part1 puzzle)
  print (part2 puzzle)
