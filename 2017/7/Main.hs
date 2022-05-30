import           Data.List (find)
import qualified Data.Set  as S

data Tower = Program String Int [Tower] deriving (Show)

main :: IO ()
main = do
  puzzleContents <- readFile "example.txt"
  let puzzle = parseInitial . words <$> lines puzzleContents
        where parseInitial (name:weight:rest) = (Program name (read weight) [],
                                                 case rest of []        -> []
                                                              (_:above) -> filter (`notElem` ",") <$> above)
  print $ part1 puzzle

part1 :: [(Tower, [String])] -> Maybe String
part1 puzzle = (\(Program n _ _) -> n) <$> treeFromPuzzle puzzle

treeFromPuzzle :: [(Tower, [String])] -> Maybe Tower
treeFromPuzzle puzzle = subTree puzzle =<< rootName
  where rootName = S.lookupMin $ allNames `S.difference` childNames
        allNames = S.fromList [n | (Program n _ _, _) <- puzzle]
        childNames = S.unions [S.fromList ns | (_, ns) <- puzzle]

subTree :: [(Tower, [String])] -> String -> Maybe Tower
subTree fragments name = (\ts -> withChildren ts . fst <$> found) =<< children
  where found = find (\(Program n _ _, _) -> n == name) fragments
        children = childSubTrees fragments . snd =<< found

childSubTrees :: [(Tower, [String])] -> [String] -> Maybe [Tower]
childSubTrees fragments names = sequence $ subTree fragments <$> names

withChildren :: [Tower] -> Tower -> Tower
withChildren ts (Program rootName rootWeight _) = Program rootName rootWeight ts
