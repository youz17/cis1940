{-# OPTIONS_GHC -Wall #-}

fib :: Integer -> Integer
fib 1 = 0
fib 2 = 1
fib n
  | n > 0 = fib (n - 1) + fib (n - 2)
  | otherwise = 1

fibs1 :: [Integer]
fibs1 = map fib [1 ..]

fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(a, b) -> (b, a + b)) (0, 1)

-- 这个是网上抄的
fibs3 :: [Integer]
fibs3 = 0 : 1 : zipWith (+) fibs3 (tail fibs3)
