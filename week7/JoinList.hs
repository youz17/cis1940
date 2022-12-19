module JoinList where

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

sizeJ :: (Sized m, Monoid m) => JoinList m b -> Int
sizeJ Empty = 0
sizeJ (Single _ _) = 1
sizeJ (Append s _ _) = getSize (size s)

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ v) = Just v
indexJ _ Empty = Nothing
indexJ n (Append _ l r) = if n <= lsize then indexJ n l else indexJ (n - lsize) r
  where
    lsize = sizeJ l

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 j = j
dropJ 1 (Single _ _) = Empty
dropJ n (Append _ l r) =
  if n > lsize then dropJ (n - lsize) r else dropJ n l +++ r
  where
    lsize = sizeJ l

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _ = Empty
takeJ 1 j@(Single _ _) = j
takeJ n (Append _ l r) =
  if n <= lsize then takeJ n l else l +++ takeJ (n - lsize) r
  where
    lsize = sizeJ l

-- for test
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x : xs) !!? 0 = Just x
(x : xs) !!? i = xs !!? (i - 1)