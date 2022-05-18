module Lib (part1, part2) where

part1 :: Integer -> Integer
part1 1    = 0
part1 cell = 1 + part1 (minimumNeighbor cell)

part2 :: Integer -> Integer
part2 puzzle = 2

minimumNeighbor :: Integer -> Integer
minimumNeighbor cell = minimum (cellNeighbors cell)

cellNeighbors :: Integer -> [Integer]
cellNeighbors cell = [cellForCoord (walkUp coord), cellForCoord (walkDown coord), cellForCoord (walkLeft coord), cellForCoord (walkRight coord)]
  where coord = (0, 0)

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
cellForCoord (x, y) = 1 + cellForCoord previousCoord
  where previousCoord
          | x > 0 && (x == -y + 1 || x == -y) = (x - 1, y)
          | x > 0 && x >= abs y = (x, y - 1)
          | y > 0 && y >= abs x = (x + 1, y)
          | x < 0 && x <= y = (x, y + 1)
          | y < 0 && abs y >= x = (x - 1, y)

walkUp :: (Int, Int) -> (Int, Int)
walkUp (x, y) = (x, y + 1)

walkDown :: (Int, Int) -> (Int, Int)
walkDown (x, y) = (x, y - 1)

walkLeft :: (Int, Int) -> (Int, Int)
walkLeft (x, y) = (x - 1, y)

walkRight :: (Int, Int) -> (Int, Int)
walkRight (x, y) = (x + 1, y)
