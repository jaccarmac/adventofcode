module Lib (inverseCaptcha, inverseHalfwayCaptcha) where

inverseCaptcha :: Integer -> Integer
inverseCaptcha number = sum (map fst (filter uncurry (==) (zip numerals (rotate 1 numerals))))
  where numerals = digits number

inverseHalfwayCaptcha :: Integer -> Integer
inverseHalfwayCaptcha number = sum (map fst (filter uncurry (==) (zip numerals (rotate halfway numerals))))
  where numerals = digits number
        halfway = length numerals `div` 2

-- Thanks to https://stackoverflow.com/a/3963286 !
digits :: Integer -> [Integer]
digits 0      = []
digits number = digits (number `div` 10) ++ [number `mod` 10]

-- Thanks to https://stackoverflow.com/a/16379034 !
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs
