{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Stream where

import Data.Function ((&))

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons v s) = v : streamToList s

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat v = Cons v (streamRepeat v)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons v s) = Cons (f v) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Cons s (streamFromSeed f (f s))

nats :: Stream Integer
nats = streamFromSeed succ 0

ruler :: Stream Integer
ruler = streamFromSeed succ 1 & streamMap div2Times
  where
    div2Times (v :: Integer) = if even v then 1 + div2Times (v `div` 2) else 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a1 s1) s2 = Cons a1 (interleaveStreams s2 s1)

-- interleaveStreams (Cons a1 s1) (Cons a2 s2) = Cons a1 (Cons a2 (interleaveStreams s1 s2)) -- 这样会栈溢出。。。

ruler' :: Stream Integer
ruler' = helper 0
  where
    helper a = interleaveStreams (streamRepeat a) (helper (succ 1))

streamMulInt :: Integer -> Stream Integer -> Stream Integer
streamMulInt n = streamMap (* n)

instance Num (Stream Integer) where
  (+) (Cons a0 s0) (Cons a1 s1) = Cons (a0 + a1) (s0 + s1)
  (*) (Cons a0 s0') s1@(Cons a1 s1') = Cons (a0 * a1) (streamMulInt a0 s1' + s0' * s1)
  abs = streamMap abs
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap negate

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

showPolynomial :: Stream Integer -> String
showPolynomial = helper 0
  where
    helper (n :: Int) (Cons v s)
      | n == 0 && v /= 0 = show v ++ restFunc n s
      | n < 20 && v /= 0 = sigStr v ++ show (abs v) ++ "x^" ++ show n ++ restFunc n s
      | otherwise = ""
    sigStr n = if n > 0 then " + " else " - "
    restFunc n = helper (succ n)