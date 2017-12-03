module Main where

import           Lib

main :: IO ()
main = do
  puzzleContents <- readFile "puzzle.txt"
  let puzzle = read puzzleContents :: Integer
  print (part1 puzzle)
  print (part2 puzzle)
