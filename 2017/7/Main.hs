import           Data.List (find)

data Tower = Program String Int [Tower] deriving (Show)

main :: IO ()
main = do
  puzzleContents <- readFile "example.txt"
  let puzzle = (parseInitial . words) <$> lines puzzleContents
        where parseInitial (name:weight:rest) = (Program name (read weight) [],
                                                 case rest of []        -> []
                                                              (_:above) -> (filter (`notElem` ",")) <$> above)
  print $ part1 puzzle

part1 :: [(Tower, [String])] -> Maybe Tower
part1 = flip subTree "tknk"

subTree :: [(Tower, [String])] -> String -> Maybe Tower
subTree fragments name = case children of Nothing -> Nothing
                                          Just ts -> (withChildren ts) . fst <$> found
  where found = find (\(Program n _ _, _) -> n == name) fragments
        children = snd <$> found >>= childSubTrees fragments

childSubTrees :: [(Tower, [String])] -> [String] -> Maybe [Tower]
childSubTrees fragments names = sequence $ (subTree fragments) <$> names

withChildren :: [Tower] -> Tower -> Tower
withChildren ts (Program rootName rootWeight _) = Program rootName rootWeight ts
