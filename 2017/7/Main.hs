import           Data.List (find)

data Tower = Program String Int [Tower] deriving (Show)

main :: IO ()
main = do
  puzzleContents <- readFile "example.txt"
  let puzzle = map (parseInitial . words) $ lines puzzleContents
        where parseInitial (name:weight:rest) = (Program name (read weight) [],
                                                 case rest of []        -> []
                                                              (_:above) -> above)
  print $ part1 puzzle

part1 :: [(Tower, [String])] -> Maybe Tower
part1 = subTree "tknk"

subTree :: String -> [(Tower, [String])] -> Maybe Tower
subTree name fragments = fst <$> found
  where found = find (\(Program n _ _, _) -> n == name) fragments

append :: Tower -> Tower -> Tower
append t (Program rootName rootWeight rootChildren) = Program rootName rootWeight $ t:rootChildren
