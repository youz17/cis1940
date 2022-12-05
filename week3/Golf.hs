{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Golf where

import Data.Foldable (find, maximumBy)
import Data.Function ((&))
import Data.Map (fromListWith, toList)

nth :: Int -> [a] -> Maybe a
nth _ [] = Nothing
nth n (x : xs)
  | n < 0 = Nothing
  | n == 0 = Just x
  | otherwise = nth (n - 1) xs

nths' :: [a] -> Int -> [a]
nths' xs n = case nth (n - 1) xs of
  Nothing -> []
  Just x -> x : nths' (drop n xs) n

ll :: [a] -> [Int]
ll xs = [1 .. (length xs)]

nths :: [a] -> Int -> [a]
nths xs n = ll xs & zip xs & filter (\(_, i) -> mod i n == 0) & map fst

skips :: [a] -> [[a]]
skips xs = ll xs & map (nths xs)

localMaxima :: [Integer] -> [Integer]
localMaxima (a : b : c : xs)
  | b > c && b > a = b : res
  | otherwise = res
  where
    res = localMaxima (b : c : xs)
localMaxima _ = []

bigToStar :: [(Int, Int)] -> Int -> Int -> Char
bigToStar xs c n = case find ((== n) . fst) xs of
  Just (_, cc) -> if cc >= c then '*' else ' '
  Nothing -> ' '

frequency :: (Ord a) => [a] -> [(a, Int)]
frequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

histogram' :: Int -> [(Int, Int)] -> String
histogram' n xs
  | n > 0 =
      map (bigToStar xs n) [0 .. 9] ++ "\n" ++ histogram' (n - 1) xs
  | otherwise = "==========\n0123456789\n"

histogram :: [Int] -> String
histogram l =
  histogram' maxCount posWithCount
  where
    posWithCount = frequency l
    maxCount = maximumBy (\(_, a) (_, b) -> compare a b) posWithCount & snd
