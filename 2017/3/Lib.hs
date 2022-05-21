module Lib (part1, part2) where

part1 :: Integer -> Integer
part1 1    = 0
part1 cell = manhattan $ coordForCell cell

part2 :: Integer -> Integer
part2 puzzle = 2

manhattan :: (Integer, Integer) -> Integer
manhattan (x, y) = abs x + abs y

minimumNeighbor :: Integer -> Integer
minimumNeighbor cell = minimum (cellNeighbors cell)

cellNeighbors :: Integer -> [Integer]
cellNeighbors cell = [cellForCoord (walkUp coord), cellForCoord (walkDown coord), cellForCoord (walkLeft coord), cellForCoord (walkRight coord)]
  where coord = coordForCell cell

cellsInRing :: Integer -> [Integer]
cellsInRing 0 = [1]
cellsInRing ring = take (fromIntegral (ringsDimension ring ^ 2 -
                         ringsDimension (ring - 1) ^ 2))
                   (drop (fromIntegral (ringsDimension (ring - 1) ^ 2)) [1..])

ringsDimension :: Integer -> Integer
ringsDimension rings = rings * 2 + 1

ringForCell :: Integer -> Integer
ringForCell cell = head (filter (\r -> cell `elem` cellsInRing r) [1..])

cellForCoord :: (Integer, Integer) -> Integer
cellForCoord (0, 0) = 1
cellForCoord (x, y) = 1 + cellForCoord previousCoord
  where previousCoord
          | x > 0 && (x == -y + 1 || x == -y) = (x - 1, y)
          | x > 0 && x >= abs y = (x, y - 1)
          | y > 0 && y >= abs x = (x + 1, y)
          | x < 0 && x <= y = (x, y + 1)
          | y < 0 && abs y >= x = (x - 1, y)

coordForCell :: Integer -> (Integer, Integer)
coordForCell cell = spiralCoordinates !! fromIntegral (cell - 1)

spiralCoordinates :: [(Integer, Integer)]
spiralCoordinates = iterate nextCoordinate (0, 0)

nextCoordinate :: (Integer, Integer) -> (Integer, Integer)
nextCoordinate (x, y)
  | x >= 0 && x == -y = (x + 1, y)
  | x > 0 && x == y = (x - 1, y)
  | x < 0 && -x == y = (x, y - 1)
  | x < 0 && x == y = (x + 1, y)
  | x > 0 && x > abs y = (x, y + 1)
  | y > 0 && y >= abs x = (x - 1, y)
  | x < 0 && x < y = (x, y - 1)
  | y < 0 && y < x = (x + 1, y)

walkUp :: (Integer, Integer) -> (Integer, Integer)
walkUp (x, y) = (x, y + 1)

walkDown :: (Integer, Integer) -> (Integer, Integer)
walkDown (x, y) = (x, y - 1)

walkLeft :: (Integer, Integer) -> (Integer, Integer)
walkLeft (x, y) = (x - 1, y)

walkRight :: (Integer, Integer) -> (Integer, Integer)
walkRight (x, y) = (x + 1, y)
