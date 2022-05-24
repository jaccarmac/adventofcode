module Lib (part1, part2) where

part1 :: [[Int]] -> Int
part1 puzzle = sum (map calc puzzle)
  where calc r = maximum r - minimum r

part2 :: [[Int]] -> Int
part2 puzzle = sum (map calc puzzle)
  where calc r = maximum [i `div` j | i <- r, j <- r, i `rem` j == 0]
