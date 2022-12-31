{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}

module JoinList where

import Buffer
import Scrabble (Score (Score), scoreString)
import Sized

data JoinList m a
  = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- gets the annotation at the root of aJoinList.
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
l +++ Empty = l
Empty +++ r = r
l +++ r = Append (tag l <> tag r) l r

-- jl 的长度
lengthJ :: JoinList m a -> Int
lengthJ Empty = 0
lengthJ (Single _ _) = 1
lengthJ (Append _ l r) = lengthJ l + lengthJ r

sizeJ :: (Sized m, Monoid m) => JoinList m b -> Int
sizeJ Empty = 0
sizeJ (Single _ _) = 1
sizeJ (Append s _ _) = getSize (size s)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ v) = Just v
indexJ n (Append _ l r) = if n < lsize then indexJ n l else indexJ (n - lsize) r
  where
    lsize = sizeJ l
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 j = j
dropJ n (Append _ l r) =
  if n > lsize then dropJ (n - lsize) r else dropJ n l +++ r
  where
    lsize = sizeJ l
dropJ _ _ = Empty -- drop n(n>=1) (Single|Empty) = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ n (Append _ l r) =
  if n <= lsize then takeJ n l else l +++ takeJ (n - lsize) r
  where
    lsize = sizeJ l
takeJ _ j = j -- take n(n>=1) j@(Single|Empty) = j

replaceJ :: (Sized b, Monoid b) => Int -> b -> a -> JoinList b a -> JoinList b a
replaceJ 0 nb na (Single _ _) = Single nb na
replaceJ n nb na (Append _ l r) =
  if n < lsize
    then replaceJ n nb na l +++ r
    else l +++ replaceJ (n - lsize) nb na r
  where
    lsize = sizeJ l
replaceJ _ _ _ _ = Empty

-- for test
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

jlFromList :: Monoid m => (a -> m) -> [a] -> JoinList m a
jlFromList _ [] = Empty
jlFromList calm [x] = Single (calm x) x
jlFromList calm l = jlFromList calm leftHalf +++ jlFromList calm rightHalf
  where
    halfIdx = length l `div` 2
    leftHalf = take halfIdx l
    rightHalf = drop halfIdx l

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x : _) !!? 0 = Just x
(_ : xs) !!? i = xs !!? (i - 1)

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

type JoinListBuffer = (JoinList (Score, Size) String)

instance Buffer JoinListBuffer where
  toString (Single _ s) = s ++ "\n" -- 这里其实是一个 fold 操作
  toString Empty = ""
  toString (Append _ l r) = toString l ++ toString r
  fromString = jlFromList calm . lines
    where
      calm s = (scoreString s, Size 1)
  line = indexJ
  replaceLine idx nl = replaceJ idx nm nl
    where
      nm = (scoreString nl, Size 1)
  numLines = sizeJ
  value j = score
    where
      (Score score, _) = tag j
