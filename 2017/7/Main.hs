data Tower = Program String Int [Tower] deriving (Show)

main :: IO ()
main = do
  puzzleContents <- readFile "example.txt"
  let puzzle = map parseInitial $ lines puzzleContents
        where parseInitial line | [n, w] <- words line = (Program n (read w) [], [])
                                | (n:w:_:cs) <- words line = (Program n (read w) [], cs)
  print puzzle
