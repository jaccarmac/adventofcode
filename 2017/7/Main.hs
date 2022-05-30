import           Data.List  (find)
import           Data.Maybe (fromMaybe)

data Tower = Program String Int [Tower] deriving (Show)

main :: IO ()
main = do
  puzzleContents <- readFile "example.txt"
  let puzzle = map (parseInitial . words) $ lines puzzleContents
        where parseInitial (name:weight:rest) = (Program name (read weight) [],
                                                 case rest of []        -> []
                                                              (_:above) -> above)
  print $ part1 puzzle

part1 :: [(Tower, [String])] -> Tower
part1 puzzle = subTree "tknk" puzzle

subTree :: String -> [(Tower, [String])] -> Tower
subTree name fragments = Program name weight children
  where (Program _ weight children) = maybe (Program "" 0 []) fst $ find finder fragments
        finder :: (Tower, [String]) -> Bool
        finder ((Program n _ _), _) = n == name

append :: Tower -> Tower -> Tower
append t (Program rootName rootWeight rootChildren) = Program rootName rootWeight $ t:rootChildren
