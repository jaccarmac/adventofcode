module Lib (part1, part2) where

import           Data.List

part1 :: [String] -> Int
part1 puzzle = occurrences goodPassphrase puzzle

part2 :: [String] -> Int
part2 puzzle = occurrences noAnagramPassphrase puzzle

occurrences :: (a -> Bool) -> [a] -> Int
occurrences predicate = length . filter predicate

goodPassphrase :: String -> Bool
goodPassphrase passphrase = length phraseWords == length (nub phraseWords)
  where phraseWords = words passphrase

noAnagramPassphrase :: String -> Bool
noAnagramPassphrase passphrase = length phraseSortedWords == length (nub phraseSortedWords)
  where phraseSortedWords = map sort (words passphrase)
