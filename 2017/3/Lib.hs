module Lib (part1, part2) where

import           Control.Applicative ((<**>))
import           Data.List           (mapAccumL)
import           Data.Map.Lazy       (Map)
import qualified Data.Map.Lazy       as Map
import           Data.Maybe          (mapMaybe)

type Coordinate = (Integer, Integer)

part1 :: Integer -> Integer
part1 1    = 0
part1 cell = manhattan $ coordForCell cell

part2 :: Integer -> Integer
part2 puzzle = head $ dropWhile (<= puzzle) bigList
  where bigList = snd $ mapAccumL sumForCoordinate (Map.singleton (0, 0) 1) spiralCoordinates
        sumForCoordinate m c = let neighborsSum = sum $ mapMaybe (`Map.lookup` m) $ cellNeighbors c
          in (Map.insertWith (const id) c neighborsSum m, neighborsSum)

manhattan :: Coordinate -> Integer
manhattan (x, y) = abs x + abs y

coordForCell :: Integer -> Coordinate
coordForCell cell = spiralCoordinates !! fromIntegral (cell - 1)

spiralCoordinates :: [Coordinate]
spiralCoordinates = iterate nextCoordinate (0, 0)

nextCoordinate :: Coordinate -> Coordinate
nextCoordinate (x, y)
  | center || bottomRight || bottomLeft || bottom = walkRight (x, y)
  | topLeft || left = walkDown (x, y)
  | topRight || top = walkLeft (x, y)
  | right = walkUp (x, y)
  where center = (x, y) == (0, 0)
        right = x > 0 && x >= abs y
        top = y > 0 && y >= abs x
        left = x < 0 && -x >= abs y
        bottom = y < 0 && -y >= abs x
        topRight = top && right
        topLeft = top && left
        bottomRight = bottom && right
        bottomLeft = bottom && left

cellNeighbors :: Coordinate -> [Coordinate]
cellNeighbors coord = pure coord <**> [walkUp, walkDown, walkLeft, walkRight, walkUpRight, walkUpLeft, walkDownLeft, walkDownRight]

walkUp :: Coordinate -> Coordinate
walkUp (x, y) = (x, y + 1)

walkDown :: Coordinate -> Coordinate
walkDown (x, y) = (x, y - 1)

walkLeft :: Coordinate -> Coordinate
walkLeft (x, y) = (x - 1, y)

walkRight :: Coordinate -> Coordinate
walkRight (x, y) = (x + 1, y)

walkUpRight :: Coordinate -> Coordinate
walkUpRight (x, y) = (x + 1, y + 1)

walkUpLeft :: Coordinate -> Coordinate
walkUpLeft (x, y) = (x - 1, y + 1)

walkDownLeft :: Coordinate -> Coordinate
walkDownLeft (x, y) = (x - 1, y - 1)

walkDownRight :: Coordinate -> Coordinate
walkDownRight (x, y) = (x + 1, y - 1)
