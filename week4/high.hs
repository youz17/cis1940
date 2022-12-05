{-# OPTIONS_GHC -Wall #-}

import Data.Function ((&))
import Data.Set qualified as Set

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun1'' :: [Integer] -> Integer
fun1'' xs = xs & filter even & map (subtract 2) & product

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

testFunc :: (Integer -> Integer) -> (Integer -> Integer) -> [(Integer, Bool)]
testFunc f f' = [2 .. 10000] & map (\x -> (x, f x == f' x)) & filter (not . snd)

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node n _ _ _) = n

insertTree :: a -> Tree a -> Tree a
insertTree v Leaf = Node 0 Leaf v Leaf
insertTree value (Node h l rootValue r)
  | leftHeight > rightHeight = Node h l rootValue (insertTree value r)
  | otherwise =
      let newLeft = insertTree value l
       in Node (treeHeight newLeft + 1) newLeft rootValue r
  where
    leftHeight = treeHeight l
    rightHeight = treeHeight r

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

xor :: [Bool] -> Bool
xor = foldl (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a l -> f a : l) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f z = foldr (flip f) z . reverse

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  let k = (n - 1) `div` 2
   in [i + j + 2 * i * j | i <- [1 .. k], j <- [i .. (k `div` i)]]
        & filter (<= k)
        & Set.fromList
        & Set.difference (Set.fromList [1 .. k])
        & Set.toList
        & map ((+ 1) . (* 2))