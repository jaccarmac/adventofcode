import           Control.Arrow ((&&&))
import           Data.List     (find, groupBy, maximumBy, minimumBy)
import           Data.Ord      (comparing)
import qualified Data.Set      as S

data Tower = Program String Int [Tower] deriving (Show)

main :: IO ()
main = do
  puzzleContents <- readFile "puzzle.txt"
  let puzzle = parseInitial . words <$> lines puzzleContents
        where parseInitial (name:weight:rest) = (Program name (read weight) [],
                                                 case rest of []        -> []
                                                              (_:above) -> filter (`notElem` ",") <$> above)
  print $ part1 puzzle
  print $ part2 puzzle

part1 :: [(Tower, [String])] -> Maybe String
part1 puzzle = (\(Program n _ _) -> n) <$> treeFromPuzzle puzzle

part2 :: [(Tower, [String])] -> Maybe Int
part2 puzzle = part2' =<< treeFromPuzzle puzzle

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

totalWeight :: Tower -> Int
totalWeight (Program _ w ts) = w + sum (totalWeight <$> ts)

balanced :: Tower -> Bool
balanced (Program _ _ []) = True
balanced (Program _ _ ts) = minimum weights == maximum weights
  where weights = totalWeight <$> ts

part2' :: Tower -> Maybe Int
part2' (Program _ w ts) | all balanced ts = pure $ snd actualWeight + difference
                        | otherwise = part2' =<< find (not . balanced) ts
                        where weights = (totalWeight &&& (\(Program _ w _) -> w)) <$> ts
                              targetWeight = head $ maximumBy (comparing length) $ groupBy (\x y -> fst x == fst y) weights
                              actualWeight = head $ minimumBy (comparing length) $ groupBy (\x y -> fst x == fst y) weights
                              difference = fst targetWeight - fst actualWeight
