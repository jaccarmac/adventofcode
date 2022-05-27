{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
  puzzleContents <- readFile "example.txt"
  let puzzle = map (parseInitial . splitChildren . splitLine . T.pack) $ lines puzzleContents
  print puzzle

splitLine :: Text -> [Text]
splitLine = T.splitOn "->"

splitChildren :: [Text] -> (Text, [Text])
splitChildren [name]            = (name, [])
splitChildren (name:children:_) = (name, map T.strip $ T.splitOn "," children)

data Tower = Program String Int [Tower] deriving (Show)

parseInitial :: (Text, [Text]) -> (Tower, [String])
parseInitial (t, cs) = let tokens = map T.strip $ T.splitOn "(" t
                           name = T.unpack $ tokens !! 0
                           weight = read $ T.unpack $ T.take (T.length (tokens !! 1) - 1) $ tokens !! 1
                       in (Program name weight [], map T.unpack cs)
