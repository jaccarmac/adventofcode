module Main where

main :: IO ()
main = do
  puzzleContents <- readFile "puzzle.txt"
  let puzzle = map read $ lines puzzleContents
  print $ part1 puzzle
  print $ part2 puzzle

part1 :: [Integer] -> [Integer]
part1 = id

data JumpState = Exited | Offsets [Integer] Integer [Integer] deriving (Show)

jump :: JumpState -> JumpState
jump Exited = Exited
jump (Offsets p 0 s) = Offsets p 1 s
jump (Offsets preceding offset succeeding)
  | fromIntegral offset > length succeeding = Exited
  | - fromIntegral offset > length preceding = Exited
  | offset > 0 = Offsets (preceding ++ [offset + 1] ++ take (fromIntegral offset - 1) succeeding) (succeeding !! (fromIntegral offset - 1)) (drop (fromIntegral offset) succeeding)
  | offset < 0 = Offsets (take (length preceding + fromIntegral offset) preceding) (preceding !! (length preceding + fromIntegral offset)) (drop (length preceding + fromIntegral offset + 1) preceding ++ [offset + 1] ++ succeeding)

part2 :: [Integer] -> [Integer]
part2 = part1
