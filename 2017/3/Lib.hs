module Lib (part1, part2) where

import qualified Data.Map.Strict as Map
import           Data.Maybe      (catMaybes)

type Coordinate = (Integer, Integer)

part1 :: Integer -> Integer
part1 1    = 0
part1 cell = manhattan $ coordForCell cell

part2 :: Integer -> Integer
part2 puzzle = 2

manhattan :: Coordinate -> Integer
manhattan (x, y) = abs x + abs y

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

cellNeighbors :: Coordinate -> Map.Map Coordinate Integer -> [Integer]
cellNeighbors coord spiral = catMaybes [Map.lookup (walkUp coord) spiral, Map.lookup (walkDown coord) spiral, Map.lookup (walkLeft coord) spiral, Map.lookup (walkRight coord) spiral]
