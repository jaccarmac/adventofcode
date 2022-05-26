module Main where

main :: IO ()
main = do
  puzzleContents <- readFile "puzzle.txt"
  let puzzle = map read $ lines puzzleContents
  print $ part1 puzzle
  print $ part2 puzzle

part1 :: [Integer] -> Integer
part1 (offset:succeeding) = jumpsUntilExit (+ 1) (Offsets [] offset succeeding)

part2 :: [Integer] -> Integer
part2 (offset:succeeding) = jumpsUntilExit onJump (Offsets [] offset succeeding)
  where onJump offset
          | offset >= 3 = offset - 1
          | otherwise = offset + 1

data JumpState = Exited | Offsets [Integer] Integer [Integer] deriving (Show)

jump :: (Integer -> Integer) -> JumpState -> JumpState
jump _ Exited          = Exited
jump f (Offsets p o s) = move o (Offsets p (f o) s)

move :: Integer -> JumpState -> JumpState
move 0 s = s
move _ Exited = Exited
move j (Offsets [] _ _) | j < 0 = Exited
move j (Offsets _ _ []) | j > 0 = Exited
move j (Offsets (p:ps) c ss) | j < 0 = move (j + 1) (Offsets ps p (c:ss))
move j (Offsets ps c (s:ss)) | j > 0 = move (j - 1) (Offsets (c:ps) s ss)

jumpsUntilExit :: (Integer -> Integer) -> JumpState -> Integer
jumpsUntilExit f s = fromIntegral $ length $ takeWhile isNotExited $ iterate (jump f) s
  where isNotExited Exited          = False
        isNotExited (Offsets _ _ _) = True
