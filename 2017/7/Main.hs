data Tower = Program String Int [Tower] deriving (Show)

main :: IO ()
main = do
  puzzleContents <- readFile "example.txt"
  let puzzle = map (parseInitial . words) $ lines puzzleContents
        where parseInitial [n, w]     = (Program n (read w) [], [])
              parseInitial (n:w:_:cs) = (Program n (read w) [], cs)
  print puzzle
