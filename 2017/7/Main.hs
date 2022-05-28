data Tower = Program String Int [Tower] deriving (Show)

main :: IO ()
main = do
  puzzleContents <- readFile "example.txt"
  let puzzle = map (parseInitial . words) $ lines puzzleContents
        where parseInitial (name:weight:rest) = (Program name (read weight) [],
                                                 case rest of []        -> []
                                                              (_:above) -> above)
  print $ part1 puzzle

part1 :: [(Tower, [String])] -> Either [(Tower, [String])] String
part1 [(Program name _ _, [])] = Right name
