module Lib (part1, part2) where

import           Data.List

part1 :: [String] -> Int
part1 puzzle = occurrences goodPassphrase puzzle

part2 :: [String] -> Integer
part2 _ = 2

occurrences :: (a -> Bool) -> [a] -> Int
occurrences predicate = length . filter predicate

goodPassphrase :: String -> Bool
goodPassphrase passphrase = length phraseWords == length (nub phraseWords)
  where phraseWords = words passphrase
