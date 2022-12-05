{-# OPTIONS_GHC -Wall #-}

import Data.Function ((&))

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = if x <= 0 then [] else (x `mod` 10) : toDigitsRev (x `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther' :: [Integer] -> [Integer]
doubleEveryOther' [] = []
doubleEveryOther' [a] = [a]
doubleEveryOther' (a : b : rest) = a : (b * 2) : doubleEveryOther' rest

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (doubleEveryOther' (reverse x))

sumDigits :: [Integer] -> Integer
sumDigits lst = sum (concatMap toDigits lst)

validate :: Integer -> Bool
validate card = (card & toDigits & doubleEveryOther & sumDigits) `mod` 10 == 0

-- validate card = sumDigits (doubleEveryOther (toDigits card)) `mod` 10 == 0
