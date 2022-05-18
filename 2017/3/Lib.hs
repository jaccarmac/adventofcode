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
  where ring = ringForCell cell
        maxInRing = cellForCoord (ring, -ring)
        distance = maxInRing - cell
        sideLength = ringsDimension ring - 1
        coord
          | distance < sideLength = (ring - distance, -ring)
          | distance < sideLength * 2 = (-ring, -ring + distance `rem` sideLength)
          | distance < sideLength * 3 = (-ring + distance `rem` sideLength, ring)
          | otherwise = (ring, ring - distance `rem` sideLength)

cellsInRing :: Integer -> [Integer]
cellsInRing 0 = [1]
cellsInRing ring = take (fromIntegral ((ringsDimension ring)^2 -
                         (ringsDimension (ring - 1))^2))
                   (drop (fromIntegral ((ringsDimension (ring - 1))^2)) [1..])

ringsDimension :: Integer -> Integer
ringsDimension rings = rings * 2 + 1

ringForCell :: Integer -> Integer
ringForCell cell = head (filter (\r -> elem cell (cellsInRing r)) [1..])

cellForCoord :: (Integer, Integer) -> Integer
cellForCoord (0, 0) = 1
cellForCoord (x, y) = 1 + cellForCoord previousCoord
  where previousCoord
          | x > 0 && (x == -y + 1 || x == -y) = (x - 1, y)
          | x > 0 && x >= abs y = (x, y - 1)
          | y > 0 && y >= abs x = (x + 1, y)
          | x < 0 && x <= y = (x, y + 1)
          | y < 0 && abs y >= x = (x - 1, y)

walkUp :: (Integer, Integer) -> (Integer, Integer)
walkUp (x, y) = (x, y + 1)

walkDown :: (Integer, Integer) -> (Integer, Integer)
walkDown (x, y) = (x, y - 1)

walkLeft :: (Integer, Integer) -> (Integer, Integer)
walkLeft (x, y) = (x - 1, y)

walkRight :: (Integer, Integer) -> (Integer, Integer)
walkRight (x, y) = (x + 1, y)
