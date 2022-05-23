module Lib (part1, part2) where

type Coordinate = (Integer, Integer)

part1 :: Integer -> Integer
part1 1    = 0
part1 cell = manhattan $ coordForCell cell

part2 :: Integer -> Integer
part2 puzzle = 2

manhattan :: Coordinate -> Integer
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

cellForCoord :: Coordinate -> Integer
cellForCoord (0, 0) = 1
cellForCoord (x, y) = 1 + cellForCoord previousCoord
  where previousCoord
          | x > 0 && (x == -y + 1 || x == -y) = (x - 1, y)
          | x > 0 && x >= abs y = (x, y - 1)
          | y > 0 && y >= abs x = (x + 1, y)
          | x < 0 && x <= y = (x, y + 1)
          | y < 0 && abs y >= x = (x - 1, y)

coordForCell :: Integer -> Coordinate
coordForCell cell = spiralCoordinates !! fromIntegral (cell - 1)

spiralCoordinates :: [Coordinate]
spiralCoordinates = iterate nextCoordinate (0, 0)

nextCoordinate :: Coordinate -> Coordinate
nextCoordinate (x, y)
  | bottomRight || bottomLeft || bottom = walkRight (x, y)
  | right = walkUp (x, y)
  | topRight || top = walkLeft (x, y)
  | topLeft || left = walkDown (x, y)
  where bottomRight = x >= 0 && x == -y
        topRight = x > 0 && x == y
        topLeft = x < 0 && -x == y
        bottomLeft = x < 0 && x == y
        right = x > 0 && x > abs y
        top = y > 0 && y > abs x
        left = x < 0 && -x > abs y
        bottom = y < 0 && -y > abs x

walkUp :: Coordinate -> Coordinate
walkUp (x, y) = (x, y + 1)

walkDown :: Coordinate -> Coordinate
walkDown (x, y) = (x, y - 1)

walkLeft :: Coordinate -> Coordinate
walkLeft (x, y) = (x - 1, y)

walkRight :: Coordinate -> Coordinate
walkRight (x, y) = (x + 1, y)
