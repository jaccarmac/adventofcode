module Lib (part1, part2) where

part1 :: Integer -> Integer
part1 1    = 0
part1 cell = 1 + part1 (minimum (cellNeighbors cell))

part2 :: Integer -> Integer
part2 puzzle = 2

cellNeighbors :: Integer -> [Integer]
cellNeighbors cell = []

cellsInRing :: Int -> [Integer]
cellsInRing 0 = [1]
cellsInRing ring = take ((ringsDimension ring)^2 -
                         (ringsDimension (ring - 1))^2)
                   (drop ((ringsDimension (ring - 1))^2) [1..])

ringsDimension :: Int -> Int
ringsDimension rings = rings * 2 + 1

ringForCell :: Integer -> Int
ringForCell cell = head (filter (\r -> elem cell (cellsInRing r)) [1..])

cellForCoord :: (Int, Int) -> Integer
cellForCoord (0, 0) = 1
cellForCoord (x, y)
  | y == - (x - 1) && y <= 0 = 1 + cellForCoord (x - 1, y)
  | x > 0 && x >= y = 1 + cellForCoord (x, y - 1)
  | y > 0 && y >= abs x = 1 + cellForCoord (x + 1, y)
  | x < 0 && x <= y = 1 + cellForCoord (x, y + 1)
  | y < 0 && abs y >= x = 1 + cellForCoord (x - 1, y)

walkUp :: (Int, Int) -> (Int, Int)
walkUp (x, y) = (x, y + 1)

walkDown :: (Int, Int) -> (Int, Int)
walkDown (x, y) = (x, y - 1)

walkLeft :: (Int, Int) -> (Int, Int)
walkLeft (x, y) = (x - 1, y)

walkRight :: (Int, Int) -> (Int, Int)
walkRight (x, y) = (x + 1, y)
