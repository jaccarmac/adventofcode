module Lib (part1, part2) where

part1 :: [[Int]] -> Int
part1 puzzle = sum (map (\r -> maximum r - minimum r) puzzle)

part2 :: [[Int]] -> Int
part2 puzzle = sum (map (\r -> maximum [i `div` j | i <- r, j <- r, i `rem` j == 0]) puzzle)
