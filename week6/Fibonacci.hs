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

newtype Matrix = Matrix ((Integer, Integer), (Integer, Integer))

instance Num Matrix where
  (+) (Matrix ((a0, a1), (a2, a3))) (Matrix ((b0, b1), (b2, b3))) = Matrix ((a0 + b0, a1 + b1), (a2 + b2, a3 + b3))
  (*) (Matrix ((a0, a1), (a2, a3))) (Matrix ((b0, b1), (b2, b3))) =
    Matrix
      ( ( a0 * b0 + a1 * b2,
          a0 * b1 + a1 * b3
        ),
        ( a2 * b0 + a3 * b2,
          a2 * b1 + a3 * b3
        )
      )
  abs = undefined
  signum = undefined
  fromInteger = undefined
  negate = undefined

fib4 :: Integer -> Integer
fib4 n = helper (Matrix ((1, 1), (1, 0)) ^ n)
  where
    helper (Matrix ((_, _), (_, v))) = v