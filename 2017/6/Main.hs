import           Data.List (mapAccumL)
import           Data.Set  (Set)
import qualified Data.Set  as Set

main :: IO ()
main = do
  puzzleContents <- readFile "puzzle.txt"
  let puzzle = map read $ words puzzleContents
  print $ part1 puzzle

part1 :: [Integer] -> Int
part1 puzzle = length $ takeWhile not $ snd $ mapAccumL hasDuplicates Set.empty $ iterate distribute puzzle

distribute :: [Integer] -> [Integer]
distribute memory = distributeFrom (maximum memory) [] memory

distributeFrom :: Integer -> [Integer] -> [Integer] -> [Integer]
distributeFrom target old (bank:rest)
  | target == bank = fillBanks bank (old ++ [0]) rest
  | otherwise = distributeFrom target (old ++ [bank]) rest

fillBanks :: Integer -> [Integer] -> [Integer] -> [Integer]
fillBanks 0 done rest = done ++ rest
fillBanks remaining done [] = fillBanks remaining [] done
fillBanks remaining done (bank:rest) = fillBanks (remaining - 1) (done ++ [bank + 1]) rest

hasDuplicates :: Set [Integer] -> [Integer] -> (Set [Integer], Bool)
hasDuplicates set memory = (Set.insert memory set, Set.member memory set)
